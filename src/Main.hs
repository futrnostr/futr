{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Codec.Serialise                      as S
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
import           System.Directory
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

  postInfo = hstack
    [ selectableText $ T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey e
    , spacer
    , selectableText $ content e
    ]

  row = hstack
    [ postInfo
    , spacer
    , button "View" (ViewPost e)
    ] `styleBasic` [padding 10, borderB 1 rowSep]

buildUI :: AppWenv -> AppModel -> AppNode
buildUI wenv model = widgetTree where
    baseLayer = viewPosts wenv model

    closeIcon = icon IconClose
        `styleBasic` [width 16, height 16, fgColor black, cursorHand]

    dialogContent = case model ^. dialog of
        NoAppDialog           -> vstack [ label "if you see this text, something went really wrong"] -- we never show this anyway
        GenerateKeyPairDialog -> generateOrImportKeyPairStack model
        ViewPostDialog        -> viewPostUI wenv model

    dialogLayer = vstack
        [ hstack
            [ filler
            , box_ [alignTop, onClick CloseDialog] closeIcon
                `nodeEnabled` (model ^. dialog /= GenerateKeyPairDialog)
            ]
        , spacer
        , dialogContent
        --, compWidget model
        ] `styleBasic` [width 500, height 400, padding 10, radius 10, bgColor darkGray]

    widgetTree = zstack
        [ baseLayer
        , box_ [alignCenter, alignMiddle] dialogLayer
            `nodeVisible` (model ^. dialog /= NoAppDialog)
            `styleBasic` [bgColor (gray & L.a .~ 0.8)]
        ]

viewPosts :: AppWenv -> AppModel -> AppNode
viewPosts wenv model = widgetTree where
    posts = vstack postRows where
      orderedPosts = (\e -> model ^? receivedEvents . ix e)
      postFade idx e = animRow where
        action = ViewPost e
        item = postRow wenv idx e
        animRow = animFadeOut_ [onFinished action] item `nodeKey` (content e)

      postRows = zipWith postFade [0..] (model ^. receivedEvents)

    footerTree = vstack
        [ hstack
            [ label "Lets put here"
            ]
        ] `styleBasic` [border 1 $ rgbHex "#FFFFFF"]

    postTree = vstack
        [ label "New Post"
        , spacer
        , vstack
            [ hstack
                [ textArea newPostInput `nodeKey` "newPost"
                    `styleBasic` [height 100]
                , filler
                , button "Post" SendPost
                    `styleBasic` [height 50]
                ]
            ]
        , spacer
        , scroll_ [scrollOverlay] $ posts `styleBasic` [padding 10]
        ] `styleBasic` [padding 10]

    identitiesTree = vstack
        [ label "Identities"
        , spacer
        , vstack
            (map (\k -> xOnlyPubKeyElem $ snd k) (model ^. keys))
        ] `styleBasic` [padding 10]

    widgetTree = vstack
        [ hstack
            [ identitiesTree
            , spacer
            , postTree
            ]
        , spacer
        , filler
        , footerTree
        ]

viewPostUI :: AppWenv -> AppModel -> AppNode
viewPostUI wenv model = widgetTree where
    k = fst $ fromJust $ model ^. currentKeys
    event = fromJust $ model ^. viewPost
    postInfo = vstack
        [ hstack
            [ button "Back" Back
            , filler
            ]
        , spacer
        , hstack
            [ selectableText $ T.pack $ exportXOnlyPubKey $ NostrTypes.pubKey event
            , spacer
            , selectableText $ content event
            ]
        ]

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
        , xOnlyPubKeyElem $ deriveXOnlyPubKey k
        ] `styleBasic` [padding 10]

selectableText :: Text -> WidgetNode AppModel AppEvent
selectableText t = textFieldD_ (WidgetValue t) [readOnly]
    `styleBasic`
       [ border 0 transparent
       , radius 0
       , bgColor $ rgbHex "#515151"
       ]

xOnlyPubKeyElem :: XOnlyPubKey -> WidgetNode AppModel AppEvent
xOnlyPubKeyElem x = hstack
    [ label "XOnlyPubKey"
    , spacer
    , textFieldD_ (WidgetValue $ T.pack $ exportXOnlyPubKey x) [readOnly]
    ]

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
    AppInit -> [ Producer tryLoadKeysFromDisk ] ++ (map (\r -> Producer $ connectRelay env r) defaultPool)
    RelayConnected r ->
        [ Model $ model & pool %~ (r {connected = True} <|)
        , Task $ subscribe env (model ^. eventFilter)
        ]
    RelayDisconnected r -> [ Model $ model & pool %~ (r {connected = False} <|)]
    AddRelay r -> [Producer $ connectRelay env r]
    ShowGenerateKeyPairDialog -> [ Model $ model & dialog .~ GenerateKeyPairDialog ]
    GenerateKeyPair -> [Producer generateNewKeyPair]
    KeyPairGenerated k ->
      [ Model $ model
        & keys .~ [ks]
        & currentKeys .~ Just ks
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
      , Task $ subscribe env ef
      , Task $ saveKeyPair k
      ] where
      pk = deriveXOnlyPubKey k
      ks = (k, pk)
      ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
    ImportSecKey ->
      [ Model $ model
        & keys .~ [ks]
        & currentKeys .~ Just ks
        & mySecKeyInput .~ ""
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
      , Task $ subscribe env ef
      ]
      where kp =
              fmap keyPairFromSecKey $
              maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
            pk = deriveXOnlyPubKey $ fromJust kp
            ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
            ks = (fromJust kp, pk)
    NoKeysFound -> [ Model $ model & dialog .~ GenerateKeyPairDialog ]
    SendPost ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    ViewPost e ->
        [ Model $ model
            & viewPost .~ (Just e)
            & dialog .~ ViewPostDialog
        ]
    Back -> [Model $ model & viewPost .~ Nothing]
    PostSent -> [Model $ model & newPostInput .~ ""]
    ReplyToPost e ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    EventAppeared e -> [Model $ model & receivedEvents %~ (e <|)]
    CloseDialog -> [Model $ model & dialog .~ NoAppDialog]

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
    let x = snd $ fromJust (model ^. currentKeys)
    let raw = case model ^. viewPost of {
        Just p  -> replyNote p (strip $ model ^. newPostInput) x now;
        Nothing -> textNote (strip $ model ^. newPostInput) x now;
    }
    atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent raw (fst $ fromJust $ model ^. currentKeys) x
    return PostSent

saveKeyPair :: KeyPair -> IO AppEvent
saveKeyPair k = do
    BS.writeFile "keys.ft" $ getKeyPair k
    putStrLn "KeyPair saved to disk"
    return NoOp

tryLoadKeysFromDisk :: (AppEvent -> IO ()) -> IO ()
tryLoadKeysFromDisk sendMsg = do
    let fp = "keys.ft"
    fe <- doesFileExist fp
    case fe of
        False -> sendMsg $ NoKeysFound
        True  -> do
            content <- BS.readFile fp :: IO BS.ByteString
            let kp = keypair $ content :: Maybe KeyPair
            case kp of
                Just k -> do
                    sendMsg $ KeyPairGenerated k
                _      -> do
                    sendMsg $ NoKeysFound

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = do
  start $ \conn -> do
    putStrLn $ "Connected to " ++ (host r) ++ ":" ++ (show (port r)) ++ isSecure
    receiveWs conn sendMsg
    sendWs (env ^. channel) conn
    sendMsg $ RelayConnected r
  where
    h = host r
    path = "/"
    start = case secure r of
        True  ->  runSecureClient h (port r) path
        False -> WS.runClient h (fromIntegral $ port r) path
    isSecure = case secure r of
        True  -> " (secure connection)"
        False -> " (insecure connection)"

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