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
import           Monomer.Widgets.Single
import qualified Network.Connection                   as Connection
import           Network.Socket
import           Network.WebSockets                   (ClientApp, Connection,
                                                       receiveData, sendClose,
                                                       sendTextData)
import qualified Network.WebSockets                   as WS
import qualified Network.WebSockets.Stream            as Stream
import           System.Directory                     (doesFileExist)
import qualified System.Exit                          as Exit
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
        NoAppDialog           -> vstack [ label "if you see this text, something went really wrong" ] -- we never show this anyway
        GenerateKeyPairDialog -> generateOrImportKeyPairStack model
        ViewPostDialog        -> viewPostUI wenv model
        ErrorReadingKeysFileDialog -> errorReadingKeysFileStack
        RelayDialog r         -> viewRelayDialog r wenv model
        NewRelayDialog        -> viewNewRelayDialog wenv model

    dialogLayer = vstack
        [ hstack
            [ filler
            , box_ [alignTop, onClick CloseDialog] closeIcon
                `nodeEnabled` (not $ elem (model ^. dialog) [GenerateKeyPairDialog, ErrorReadingKeysFileDialog])
            ]
        , spacer
        , dialogContent
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
            ([filler] ++ connections ++ [spacer, addRelay])
        ] `styleBasic` [padding 10] where
            connections = map (\r -> box_ [onClick $ ShowRelayDialog r] (tooltip (T.pack $ relayName r) (viewCircle r)
                `styleBasic` [cursorIcon CursorHand])) (model ^. pool)
            addRelay = box_ [alignTop, onClick AddNewRelayDialog] $ tooltip (T.pack $ "Add new relayy") $ addIcon
            addIcon = icon IconPlus `styleBasic` [width 16, height 16, fgColor black, cursorHand]

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
            (map (\k -> xOnlyPubKeyElem $ snd' k) (model ^. keys))
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
    k = fst' $ mainKey $ model ^. keys
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

viewRelayDialog :: Relay -> AppWenv -> AppModel -> AppNode
viewRelayDialog r wenv model =
    vstack
        [ hstack
            [ label "Host"
            , spacer
            , textField relayHostInput `nodeKey` "relayHostInput"
            ]
        , spacer
        , hstack
            [ label "Port"
            , spacer
            , numericField relayPortInput `nodeKey` "relayPortInput"
            ]
        , spacer
        , hstack
            [ label "Secure connection"
            , spacer
            , checkbox relaySecureInput `nodeKey` "relaySecureCheckbox"
            ]
        , spacer
        , hstack
            [ label "Readable"
            , spacer
            , checkbox relayReadableInput `nodeKey` "relayReadableCheckbox"
            ]
        , spacer
        , hstack
            [ label "Writable"
            , spacer
            , checkbox relayWritableInput `nodeKey` "relayWriteableCheckbox"
            ]
        , filler
        , hstack
            [ filler
            , label "Connection Status"
            , spacer
            , button connStatus connButton
            ]
        , spacer
        , button "Save" (UpdateRelay r)
        ] `styleBasic` [padding 10] where
            connStatus = if connected r then "Disconnect" else "Connect"
            connButton = if connected r then (DisconnectRelay r) else (ConnectRelay r)

viewNewRelayDialog :: AppWenv -> AppModel -> AppNode
viewNewRelayDialog wenv model =
    vstack
        [ hstack
            [ label "Host"
            , spacer
            , textField relayHostInput `nodeKey` "relayHostInput"
            ]
        , spacer
        , hstack
            [ label "Port"
            , spacer
            , numericField relayPortInput `nodeKey` "relayPortInput"
            ]
        , spacer
        , hstack
            [ label "Secure connection"
            , spacer
            , checkbox relaySecureInput `nodeKey` "relaySecureCheckbox"
            ]
        , spacer
        , hstack
            [ label "Readable"
            , spacer
            , checkbox relayReadableInput `nodeKey` "relayReadableCheckbox"
            ]
        , spacer
        , hstack
            [ label "Writable"
            , spacer
            , checkbox relayWritableInput `nodeKey` "relayWriteableCheckbox"
            ]
        , spacer
        , button "Add relay" AddRelay
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

errorReadingKeysFileStack :: AppNode
errorReadingKeysFileStack =
    vstack [ label "ERROR: Keys file could not be read." ]

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
    ConnectRelay r ->
        [ Producer $ connectRelay env r
        , Model $ model & dialog .~ NoAppDialog
        ]
    DisconnectRelay r ->
        [ Task $ disconnectRelay env r
        , Model $ model & dialog .~ NoAppDialog
        ]
    UpdateRelay r -> []
    AppInit ->
        [ Producer tryLoadKeysFromDisk
        ] ++ (map (\r -> Producer $ connectRelay env r) (model ^. pool) )
    RelayConnected r ->
        [ Model $ model & pool .~ newPool
        , Task $ subscribe env (model ^. eventFilter)
        ] where
            newPool = r : (poolWithoutRelay (model ^. pool) r)
    RelayDisconnected r -> [ Model $ model & pool .~ newPool ] where
        newPool = (r {connected = False}) : (poolWithoutRelay (model ^. pool) r)
    AddRelay ->
        [ Producer $ connectRelay env r
        , Model $ model
            & dialog .~ NoAppDialog
            & relayHostInput .~ ""
            & relayPortInput .~ 433
            & relaySecureInput .~ True
            & relayReadableInput .~ True
            & relayWritableInput .~ True
        ] where
            r = Relay
                { host = T.unpack $ model ^. relayHostInput
                , port = fromIntegral $ model ^. relayPortInput
                , secure = model ^. relaySecureInput
                , readable = model ^. relayReadableInput
                , writable = model ^. relayWritableInput
                , connected = False
                }
    ShowRelayDialog r ->
        [ Model $ model
            & dialog .~ RelayDialog r
            & relayHostInput .~ (T.pack $ host r)
            & relayPortInput .~ (fromIntegral $ port r)
            & relaySecureInput  .~ secure r
            & relayReadableInput .~ readable r
            & relayWritableInput .~ writable r
        ]
    AddNewRelayDialog ->
        [ Model $ model
            & dialog .~ NewRelayDialog
        ]
    ShowGenerateKeyPairDialog -> [ Model $ model & dialog .~ GenerateKeyPairDialog ]
    KeyPairsLoaded ks ->
        [ Model $ model
            & keys .~ ks
            & eventFilter .~ ef
            & dialog .~ NoAppDialog
        ] where
        pk = deriveXOnlyPubKey $ fst' $ mainKey ks
        ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
    GenerateKeyPair -> [Producer generateNewKeyPair]
    KeyPairGenerated k ->
      [ Model $ model
        & keys .~ ks : dk
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
      , Task $ saveKeyPairs $ ks : dk
      ] where
      pk = deriveXOnlyPubKey k
      ks = (k, pk, True)
      ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
      dk = disableKeys $ model ^. keys
    ImportSecKey ->
      [ Model $ model
        & keys .~ [ks]
        & mySecKeyInput .~ ""
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
      ]
      where kp =
              fmap keyPairFromSecKey $
              maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
            pk = deriveXOnlyPubKey $ fromJust kp
            ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
            ks = (fromJust kp, pk, True)
    NoKeysFound -> [ Model $ model & dialog .~ GenerateKeyPairDialog ]
    ErrorReadingKeysFile -> [ Model $ model & dialog .~ ErrorReadingKeysFileDialog ]
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
    let ks = mainKey $ model ^. keys
    let x = snd' ks
    let raw = case model ^. viewPost of {
        Just p  -> replyNote p (strip $ model ^. newPostInput) x now;
        Nothing -> textNote (strip $ model ^. newPostInput) x now;
    }
    atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent raw (fst' ks) x
    return PostSent

saveKeyPairs :: [Keys] -> IO AppEvent
saveKeyPairs ks = do
    LazyBytes.writeFile "keys.ft" $ encode ks
    putStrLn "KeyPairs saved to disk"
    return NoOp

tryLoadKeysFromDisk :: (AppEvent -> IO ()) -> IO ()
tryLoadKeysFromDisk sendMsg = do
    let fp = "keys.ft"
    fe <- doesFileExist fp
    case fe of
        False -> sendMsg $ NoKeysFound
        True  -> do
            content <- LazyBytes.readFile fp
            let ks = decode content :: Maybe [Keys]
            case ks of
                Just [] -> do
                    sendMsg $ NoKeysFound
                Just k -> do
                    sendMsg $ KeyPairsLoaded k
                _      -> do
                    sendMsg $ ErrorReadingKeysFile

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = do
  putStrLn $ "trying.... to connect to " ++ relayName r
  start $ \conn -> do
    let r' = r { connected = True }
    putStrLn $ "Connected to " ++ relayName r
    sendMsg $ RelayConnected r'
    if readable r then receiveWs r' conn sendMsg else sendMsg NoOp
    if writable r then sendWs (env ^. channel) r' conn sendMsg else sendMsg NoOp
  where
    h = host r
    path = "/"
    start = case secure r of
        True  ->  runSecureClient h (port r) path
        False -> WS.runClient h (fromIntegral $ port r) path
    isSecure = case secure r of
        True  -> " (secure connection)"
        False -> " (insecure connection)"

disconnectRelay :: AppEnv -> Relay -> IO AppEvent
disconnectRelay env r = if not $ connected r then return NoOp else do
    atomically $ writeTChan (env ^. channel) $ Disconnect r
    return NoOp

receiveWs :: Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
receiveWs r conn sendMsg = void . forkIO . forever $ do
    msg <- Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString)
    case msg of
        Left ex    -> do
            sendMsg $ RelayDisconnected r
            Exit.exitSuccess -- @todo I don't know better
        Right msg' -> case decode msg' of
            Just m -> do
                sendMsg $ EventAppeared $ extractEventFromServerResponse m
            Nothing -> do
                putStrLn $ "Could not decode server response: " ++ show msg'
                sendMsg $ NoOp

sendWs :: TChan ServerRequest -> Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
sendWs broadcastChannel r conn sendMsg = do
    channel <- atomically $ dupTChan broadcastChannel
    forever $ do
        msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException ServerRequest)
        case msg of
            Left ex -> do
                sendMsg $ RelayDisconnected r
                Exit.exitSuccess -- @todo I don't know better
            Right msg' -> do
                case msg' of
                    Disconnect r' ->
                        if r' == r then
                            WS.sendClose conn $ T.pack "Bye!"
                        else return ()
                    _            ->
                        WS.sendTextData conn $ encode msg'

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

main :: IO ()
main = do
  channel <- atomically newBroadcastTChan
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

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

mainKey :: [Keys] -> Keys
mainKey ks = head $ filter (\k -> third k == True) ks

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\k -> (fst' k, snd' k, False)) ks

poolWithoutRelay :: [Relay] -> Relay -> [Relay]
poolWithoutRelay p r = p' where
    p' = filter (\r' -> not $ r `sameRelay` r') p where
        sameRelay a b = host a == host b && port a == port b

viewCircle :: Relay -> WidgetNode AppModel AppEvent
viewCircle r = defaultWidgetNode "circlesGrid" widget where
    widget = createSingle () def {
        singleGetSizeReq = getSizeReq,
        singleRender = render
    }

    getSizeReq wenv node = (fixedSize 20, fixedSize 20)

    render wenv node renderer = do
        drawCircle renderer vp r
        where
          style = currentStyle wenv node
          vp = getContentArea node style

drawCircle
  :: Renderer -> Rect -> Relay -> IO ()
drawCircle renderer vp r = do
  let offsetX = -3
  let offsetY = 0
  let color = if connected r then paleGreen else orange
  let colorFill = color & L.a .~ 0.3
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer colorFill
  renderEllipse renderer (rect offsetX offsetY)
  fill renderer
  stroke renderer
  where
    size = 15
    rect ox oy = Rect rx ry size size where
        rx = vp ^. L.x + vp ^. L.w + ox - size
        ry = vp ^. L.y + oy
