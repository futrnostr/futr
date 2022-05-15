{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                    as Exception
import           Control.Lens
import           Control.Monad                        (forever, mzero, unless, void)
import           Control.Monad.STM                    (atomically)
import           Control.Monad.Trans                  (lift, liftIO)
import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Crypto.Schnorr
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import           Data.Either                          (fromRight)
import           Data.List                            (find)
import           Data.Maybe
import           Data.Monoid                          (mconcat)
import           Data.Text                            (Text, strip)
import qualified Data.Text                            as T
import           Monomer
import           Monomer.Widgets.Single
import qualified Network.Connection                   as Connection
import           Network.Socket
import           Network.WebSockets                   (ClientApp, Connection,
                                                       receiveData, sendClose,
                                                       sendTextData)
import qualified Network.WebSockets                   as WS
import qualified Network.WebSockets.Stream            as Stream
import           System.Directory                     (doesFileExist)
import           Wuss

import           AppTypes
import           Helpers
import           NostrFunctions
import           NostrTypes
import           UI
import           Widgets.Profile

import           Debug.Trace as Trace

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
    KeysSelected mks ->
      case mks of
        Just ks ->
          [ Model $ model
            & selectedKeys .~ mks
            & keys .~ keys'
            & eventFilter .~ ef
            & receivedEvents .~ []
          , Task $ saveKeyPairs keys'
          , Task $ unsubscribe env (model ^. currentSub)
          , Task $ subscribe env ef
          ] where
            keys' = switchEnabledKeys ks (model ^. keys)
            pk = snd' $ ks
            ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
        Nothing -> []
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
            & pool .~ newPool
        ] where
            r = Relay
                { host = T.unpack $ model ^. relayHostInput
                , port = fromIntegral $ model ^. relayPortInput
                , secure = model ^. relaySecureInput
                , readable = model ^. relayReadableInput
                , writable = model ^. relayWritableInput
                , connected = False
                }
            newPool = r : (poolWithoutRelay (model ^. pool) r)
    ShowDialog d ->
      case d of
        RelayDialog r ->
          [ Model $ model
              & dialog .~ RelayDialog r
              & relayReadableInput .~ readable r
              & relayWritableInput .~ writable r
          ]
        NewRelayDialog ->
          [ Model $ model
              & dialog .~ NewRelayDialog
              & relayHostInput .~ ""
              & relayPortInput .~ 433
              & relaySecureInput  .~ True
              & relayReadableInput .~ True
              & relayWritableInput .~ True
          ]
        GenerateKeyPairDialog ->
          [ Model $ model & dialog .~ GenerateKeyPairDialog ]
        _ ->
          []
    Subscribed subId ->
      [ Model $ model & currentSub .~ subId ]
    KeyPairsLoaded ks ->
        [ Model $ model
          & keys .~ ks
          & selectedKeys .~ Just mk
          & eventFilter .~ ef
          & dialog .~ NoAppDialog
          & receivedEvents .~ []
        , Task $ subscribe env ef
        ] where
        mk = mainKeys ks
        pk = snd' $ mk
        ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
    GenerateKeyPair -> [ Producer generateNewKeyPair ]
    KeyPairGenerated k ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
        & receivedEvents .~ []
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env (model ^. currentSub)
      , Task $ subscribe env ef
      ] where
      pk = deriveXOnlyPubKey k
      ks = (k, pk, True)
      ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
      dk = disableKeys $ model ^. keys
    ImportSecKey ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & mySecKeyInput .~ ""
        & eventFilter .~ ef
        & dialog .~ NoAppDialog
        & receivedEvents .~ []
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env (model ^. currentSub)
      , Task $ subscribe env (model ^. eventFilter)
      ]
      where kp =
              fromJust $
              fmap keyPairFromSecKey $
              maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
            pk = deriveXOnlyPubKey $ kp
            ef = Just $ EventFilter {filterPubKey = pk, followers = [pk]}
            ks = (kp, pk, True)
            dk = disableKeys $ model ^. keys
    NoKeysFound -> [ Model $ model & dialog .~ GenerateKeyPairDialog ]
    ErrorReadingKeysFile -> [ Model $ model & dialog .~ ErrorReadingKeysFileDialog ]
    SendPost ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    ViewPost re ->
      [ Model $ model
          & viewPost .~ (Just re)
          & dialog .~ ViewPostDialog
      ]
    ViewProfile ->
      [ Model $ model & currentView .~ ProfileView ]
    Back ->
      [ Model $ model
        & viewPost .~ Nothing
        & currentView .~ PostsView
        & dialog .~ NoAppDialog
      ]
    PostSent -> [ Model $ model & newPostInput .~ "" ]
    ReplyToPost e ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    EventAppeared e r ->
      [ Model $ model & receivedEvents .~ addReceivedEvent (model ^. receivedEvents) e r ]
    CloseDialog -> [ Model $ model & dialog .~ NoAppDialog ]

addReceivedEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addReceivedEvent re e r = addedEvent : newList
    where
        addedEvent = case find (pred e) re of
            Just (e', rs) -> (e', r : filter (\r' -> not $ r' `sameRelay` r) rs)
            _             -> (e, [r])
        newList = filter (not . pred e) re
        pred e' re' = e' == fst re'

subscribe :: AppEnv -> Maybe EventFilter -> IO AppEvent
subscribe env mfilter = do
    case mfilter of
        Just f -> do
            subId <- genSubscriptionId
            atomically $ writeTChan (env ^. channel) $ RequestRelay subId f
            return $ Subscribed subId
        _      ->
            return NoOp

unsubscribe :: AppEnv -> Text -> IO AppEvent
unsubscribe env subId = do
  atomically $ writeTChan (env ^. channel) $ Close subId
  return NoOp

handleNewPost :: AppEnv -> AppModel -> IO AppEvent
handleNewPost env model = do
    now <- getCurrentTime
    let ks = fromJust $ model ^. selectedKeys
    let x = snd' ks
    let raw = case model ^. viewPost of {
        Just p  -> replyNote (fst p) (strip $ model ^. newPostInput) x now;
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
    if not fe then sendMsg $ NoKeysFound
    else do
        content <- LazyBytes.readFile fp
        case decode content :: Maybe [Keys] of
            Just [] -> do
                sendMsg $ NoKeysFound
            Just k -> do
                sendMsg $ KeyPairsLoaded k
            _      -> do
                sendMsg $ ErrorReadingKeysFile

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = if connected r then return () else do
  putStrLn $ "trying to connect to " ++ relayName r ++ " ..."
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

disconnectRelay :: AppEnv -> Relay -> IO AppEvent
disconnectRelay env r = if not $ connected r then return NoOp else do
    atomically $ writeTChan (env ^. channel) $ Disconnect r
    return NoOp

receiveWs :: Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
receiveWs r conn sendMsg = void . forkIO $ void . runMaybeT $ forever $ do
    msg <- lift (Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString))
    case msg of
        Left ex    -> do
            liftIO $ putStrLn $ "Connection to " ++ relayName r ++ " closed"
            lift $ sendMsg $ RelayDisconnected r
            mzero
        Right msg' -> case decode msg' of
            Just m -> do
                lift $ sendMsg $ EventAppeared (extractEventFromServerResponse m) r
            Nothing -> do
                lift $ putStrLn $ "Could not decode server response: " ++ show msg'
                lift $ sendMsg $ NoOp

sendWs :: TChan ServerRequest -> Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
sendWs broadcastChannel r conn sendMsg = do
    channel <- atomically $ dupTChan broadcastChannel
    forever $ do
        msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException ServerRequest)
        case msg of
            Left ex -> sendMsg $ RelayDisconnected r
            Right msg' -> do
                case msg' of
                    Disconnect r' ->
                        if r' `sameRelay` r then do
                            WS.sendClose conn $ T.pack "Bye!"
                        else return ()
                    _            ->
                        WS.sendTextData conn $ encode msg'

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\k -> (fst' k, snd' k, False)) ks

switchEnabledKeys :: Keys -> [Keys] -> [Keys]
switchEnabledKeys k ks = map (\k' -> if k' == k
    then (fst' k', snd' k', True)
    else (fst' k', snd' k', False)
  ) ks

main :: IO ()
main = do
  channel <- atomically newBroadcastTChan
  startApp def (handleEvent $ AppEnv channel) (buildUI channel) config
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
