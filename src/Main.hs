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
import           Data.Text                            (Text, strip)
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
  rowSep = rgbaHex "#A9A9A9" 0.5
  rowBg = wenv ^. L.theme . L.userColorMap . at "rowBg" . non def

  postInfo = hstack [
      label (T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey e) `styleBasic` [width 100],
      spacer,
      label $ content e
    ] `styleBasic` [cursorHand]

  row = hstack [
      postInfo,
      spacer,
      button "View" (ViewPost e)
    ] `styleBasic` [padding 10, borderB 1 rowSep]
      --`styleHover` [bgColor rowBg]

buildUI :: AppWenv -> AppModel -> AppNode
buildUI wenv model =
  case view myKeyPair model of
    Just k ->
        case model ^. viewPost of
            Just p -> viewPostUI wenv model k p
            Nothing -> viewPosts wenv model k
    Nothing -> generateOrImportKeyPairStack model

viewPosts :: AppWenv -> AppModel -> KeyPair -> AppNode
viewPosts wenv model k = widgetTree where
    posts = vstack postRows where
      orderedPosts = (\e -> model ^? receivedEvents . ix e)
      postFade idx e = animRow where
        action = ViewPost e
        item = postRow wenv idx e
        animRow = animFadeOut_ [onFinished action] item `nodeKey` (content e)

      postRows = zipWith postFade [0..] (model ^. receivedEvents)

    widgetTree = vstack
        [ label "New Post"
        , spacer
        , hstack
            [ textArea newPostInput `nodeKey` "newPost"
                `styleBasic` [height 100]
            , filler
            , button "Post" SendPost
            ]
        , spacer
        , scroll_ [scrollOverlay] $ posts `styleBasic` [padding 10]
        , spacer
        , label "XOnlyPubKey"
        , spacer
        , label $ T.pack $ exportXOnlyPubKey $ deriveXOnlyPubKey k
        ] `styleBasic` [padding 10]

viewPostUI :: AppWenv -> AppModel -> KeyPair -> Event -> AppNode
viewPostUI wenv model k event = widgetTree where
    postInfo = hstack [
        button "Back" Back,
        spacer,
        label (T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey event) `styleBasic` [width 100],
        spacer,
        label $ content event
      ] `styleBasic` [cursorHand, textTop]

    widgetTree = vstack
        [ scroll_ [scrollOverlay] $ postInfo `styleBasic` [padding 10, textTop]
        , filler
        , hstack
            [ textArea newPostInput
                `nodeKey` "replyPost"
                `styleBasic` [height 100]
            , filler
            , button "Reply" (ReplyToPost event)
            ]
        , spacer
        , label "XOnlyPubKey"
        , spacer
        , label $ T.pack $ exportXOnlyPubKey $ deriveXOnlyPubKey k
        ] `styleBasic` [padding 10]

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
    SendPost ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    ViewPost e -> [Model $ model & viewPost .~ (Just e)]
    Back -> [Model $ model & viewPost .~ Nothing]
    PostSent -> [Model $ model & newPostInput .~ ""]
    ReplyToPost e ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
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
    let raw = case model ^. viewPost of {
        Just p  -> replyNote p (strip $ model ^. newPostInput) x now;
        Nothing -> textNote (strip $ model ^. newPostInput) x now;
    }
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
      , appTheme customDarkTheme
      , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
      , appInitEvent AppInit
      , appRenderOnMainThread
      ]

{- for debugging json parsers
main = do
    let e = "[\"EVENT\",\"044039d07ff47f100e29debaec66a3cd35e02b0cb849a3bce2cfd8bc0a1629f1\",{\"id\":\"763500a66f5ed2cc3bf77879082dc406a119ad177d12c2727e671a397a60fcfb\",\"pubkey\":\"1702e3b17de25d9fd63d80fb1a2394a26239cb2a747b893e82f776704d888c4b\",\"created_at\":1649731192,\"kind\":1,\"tags\":[[\"e\",\"d87c30cd198635dad4e6981b907fa4ea2608a6e675844ec798f85ca6bafa2a34\",\"\"]],\"content\":\"ff\",\"sig\":\"54436b7cd3afbb5c9bb7e3da455725a27c8f064acc52febe9d031918d07135acbd001192751e19579ec7cdbe76ff36631e26f02f25da94a5c5a9cc6f2ffe97c8\"}]\n";
    let d = parseE e
    putStrLn $ show d

parseE :: LazyBytes.ByteString -> Either String ServerResponse
parseE = eitherDecode
-}
customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBg" ?~ rgbHex "#656565"
  & L.userColorMap . at "replyBg" ?~ rgbHex "#555555"
  & L.userColorMap . at "replyFg" ?~ rgbHex "#909090"