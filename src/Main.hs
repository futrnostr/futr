{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                    as Exception
import           Control.Lens
import           Control.Monad                        (forever, unless, void)
import           Control.Monad.STM                    (atomically)
import           Control.Monad.Trans                  (liftIO)
import           Crypto.Schnorr
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import           Data.Either                          (fromRight)
import           Data.Maybe
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Monomer
import qualified Monomer.Lens                         as L
import qualified Network.Connection                   as Connection
import           Network.Socket
import           Network.WebSockets                   (ClientApp, Connection,
                                                       receiveData, sendClose,
                                                       sendTextData)
import qualified Network.WebSockets                   as WS
import qualified Network.WebSockets.Stream            as Stream
import           TextShow
import           Wuss

import           AppTypes
import           NostrFunctions
import           NostrTypes

type AppWenv = WidgetEnv AppModel AppEvent

type AppNode = WidgetNode AppModel AppEvent

postRow :: AppWenv -> Int -> Event -> AppNode
postRow wenv idx e = row where
  sectionBg = wenv ^. L.theme . L.sectionColor
  rowSep = rgbaHex "#A9A9A9" 0.5
  rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def

  replyBg = wenv ^. L.theme . L.userColorMap . at "replyBg" . non def
  replyFg = wenv ^. L.theme . L.userColorMap . at "replyFg" . non def
  replyIcon action = button remixReplyLine action
    `styleBasic` [textFont "Remix", textMiddle, textColor replyFg, bgColor transparent, border 0 transparent]
    `styleHover` [bgColor replyBg]
    `styleFocus` [bgColor (sectionBg & L.a .~ 0.5)]
    `styleFocusHover` [bgColor replyBg]

  postInfo = hstack [
      label (T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey e) `styleBasic` [width 100],
      spacer,
      label $ content e
    ] `styleBasic` [cursorHand]

  row = hstack [
      postInfo,
      replyIcon (ReplyToPost e)
    ] `styleBasic` [padding 10, borderB 1 rowSep]
      `styleHover` [bgColor rowBg]

buildUI :: AppWenv -> AppModel -> AppNode
buildUI wenv model =
  case view myKeyPair model of
    Just k -> widgetTree where
      sectionBg = wenv ^. L.theme . L.sectionColor

      posts = vstack postRows where
        orderedPosts = (\e -> model ^? receivedEvents . ix e)
        postFade idx e = animRow where
          action = ReplyToPost e
          item = postRow wenv idx e
          animRow = animFadeOut_ [onFinished action] item `nodeKey` (content e)

        postRows = zipWith postFade [0..] (model ^. receivedEvents)

      widgetTree = vstack
        [ label "Hello, nostr"
        , spacer
        , hstack
            [ label $ "New Post"
            , spacer
            , textField newPostInput `nodeKey` "newPost"
            , spacer
            , button "Post" SendPost
            ]
        , spacer
        , scroll_ [scrollOverlay] $ posts `styleBasic` [padding 10]
        , spacer
        , label "XOnlyPubKey"
        , spacer
        , label $ T.pack $ exportXOnlyPubKey $ deriveXOnlyPubKey k
        ] `styleBasic` [padding 10]

    Nothing -> generateOrImportKeyPairStack model

generateOrImportKeyPairStack :: AppModel -> AppNode
generateOrImportKeyPairStack model =
  vstack
    [ label "Welcome to nostr"
    , spacer
    , label "To get started, you need a valid key pair first"
    , spacer
    , hstack
        [ label "Generate new key pair"
        , spacer
        , button "Generate" GenerateKeyPair
        ]
    , spacer
    , label "or import an existing private key"
    , spacer
    , hstack
        [ textField mySecKeyInput `nodeKey` "importmyprivatekey"
        , spacer
        , button "Import" ImportSecKey `nodeEnabled` isValidPrivateKey
        ]
    ] `styleBasic`
  [padding 10]
  where
    isValidPrivateKey =
      isJust $ maybe Nothing secKey $ decodeHex $ view mySecKeyInput model

handleEvent ::
     AppEnv
  -> AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent env wenv node model evt =
  case evt of
    NoOp -> []
    AppInit -> map (\r -> Producer $ connectRelay env r) defaultPool
    RelayConnected r ->
        [ Model $ model & pool %~ (r {connected = True} <|)
        , Task $ subscribe env (model ^. eventFilter)
        ]
    RelayDisconnected r -> [ Model $ model & pool %~ (r {connected = False} <|)]
    AddRelay r -> [Producer $ connectRelay env r]
    GenerateKeyPair -> [Producer generateNewKeyPair]
    KeyPairGenerated k ->
      [ Model $ model
        & myKeyPair .~ (Just k)
        & myXOnlyPubKey .~ (Just pk)
        & eventFilter .~ ef
      , Task $ subscribe env ef
      ] where
      pk = deriveXOnlyPubKey k
      ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
    ImportSecKey ->
      [ Model $ model
        & myKeyPair .~ kp
        & myXOnlyPubKey .~ (fmap deriveXOnlyPubKey kp)
        & mySecKeyInput .~ ""
        & eventFilter .~ ef
      , Task $ subscribe env ef
      ]
      where kp =
              fmap keyPairFromSecKey $
              maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
            pk = deriveXOnlyPubKey $ fromJust kp
            ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
    SendPost -> [Task $ handleNewPost env model]
    PostSent -> [Model $ model & newPostInput .~ ""]
    ReplyToPost e -> []
    EventAppeared e -> [Model $ model & receivedEvents %~ (e <|)]

subscribe :: AppEnv -> Maybe EventFilter -> IO AppEvent
subscribe env mfilter = do
    putStrLn $ show mfilter
    case mfilter of
        Just f -> do
            subId <- genSubscriptionId
            atomically $ writeTChan (env ^. channel) $ RequestRelay subId f
            return NoOp
        _      ->
            return NoOp

handleNewPost :: AppEnv -> AppModel -> IO AppEvent
handleNewPost env model = do
    now <- getCurrentTime
    let x = fromJust (model ^. myXOnlyPubKey)
    let raw = textNote (model ^. newPostInput) x now
    atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent raw (fromJust $ model ^. myKeyPair) x
    return PostSent

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = do
  --runSecureClient h p path $ \conn -> do
  WS.runClient h p path $ \conn -> do
    putStrLn $ "Connected to " ++ (host r) ++ ":" ++ (show (port r))
    receiveWs conn sendMsg
    sendWs (env ^. channel) conn
  where
    h = host r
    p = fromIntegral $ port r
    path = "/"

receiveWs :: WS.Connection -> (AppEvent -> IO ()) -> IO ()
receiveWs conn sendMsg = void . forkIO . forever $ do
    msg <- WS.receiveData conn
    case decode msg of
        Just m -> do
            sendMsg $ EventAppeared $ extractEventFromServerResponse m
        Nothing -> do
            putStrLn $ "Could not decode server response: " ++ show msg
            sendMsg $ NoOp

sendWs :: TChan ServerRequest -> WS.Connection -> IO ()
sendWs channel conn = forever $ do
    msg <- atomically $ readTChan channel
    putStrLn $ show msg
    putStrLn $ show $ encode msg
    WS.sendTextData conn $ encode $ msg

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

main :: IO ()
main = do
  channel <- newTChanIO
  startApp def (handleEvent $ AppEnv channel) buildUI config
  where
    config =
      [ appWindowTitle "FuTr"
      , appTheme darkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appInitEvent AppInit
      , appRenderOnMainThread
      ]
