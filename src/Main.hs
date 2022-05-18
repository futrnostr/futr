{-# LANGUAGE FlexibleInstances    #-}
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
import           Data.List                            (find, sortBy)
import qualified Data.Map                             as Map
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
import           NostrTypes                           as NT
import           UI
import           Widgets.Profile

import           Debug.Trace as Trace

handleEvent
  :: AppEnv
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
            ef = eventFilterFromKeys ks (model ^. AppTypes.followers)
        Nothing -> []
    ConnectRelay r ->
      [ Producer $ connectRelay env r
      , Model $ model & dialog .~ Nothing
      ]
    DisconnectRelay r ->
      [ Task $ disconnectRelay env r
      , Model $ model & dialog .~ Nothing
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
    RelayDisconnected r ->
      [ Model $ model & pool .~ newPool ]
      where
        newPool = (r {connected = False}) : (poolWithoutRelay (model ^. pool) r)
    AddRelay ->
      [ Producer $ connectRelay env r
      , Model $ model
          & dialog .~ Nothing
          & relayModel . relayHostInput .~ ""
          & relayModel . relayPortInput .~ 433
          & relayModel . relaySecureInput .~ True
          & relayModel . relayReadableInput .~ True
          & relayModel . relayWritableInput .~ True
          & pool .~ newPool
      ]
      where
        r = Relay
            { host = T.unpack $ model ^. relayModel . relayHostInput
            , port = fromIntegral $ model ^. relayModel . relayPortInput
            , secure = model ^. relayModel . relaySecureInput
            , readable = model ^. relayModel . relayReadableInput
            , writable = model ^. relayModel . relayWritableInput
            , connected = False
            }
        newPool = r : (poolWithoutRelay (model ^. pool) r)
    ShowDialog d ->
      case d of
        RelayDialog r ->
          [ Model $ model
            & dialog .~ Just (RelayDialog r)
            & relayModel . relayReadableInput .~ readable r
            & relayModel . relayWritableInput .~ writable r
          ]
        NewRelayDialog ->
          [ Model $ model
            & dialog .~ Just NewRelayDialog
            & relayModel . relayHostInput .~ ""
            & relayModel . relayPortInput .~ 433
            & relayModel . relaySecureInput  .~ True
            & relayModel . relayReadableInput .~ True
            & relayModel . relayWritableInput .~ True
          ]
        GenerateKeyPairDialog ->
          [ Model $ model & dialog .~ Just GenerateKeyPairDialog ]
        _ -> []
    Subscribed subId ->
      [ Model $ model & currentSub .~ subId ]
    KeyPairsLoaded ks ->
      [ Model $ model
        & keys .~ ks
        & selectedKeys .~ Just mk
        & eventFilter .~ ef
        & dialog .~ Nothing
        & receivedEvents .~ []
      , Task $ subscribe env ef
      ]
      where
        mk = mainKeys ks
        ef = eventFilterFromKeys mk (model ^. AppTypes.followers)
    GenerateKeyPair ->
      [ Producer generateNewKeyPair ]
    KeyPairGenerated k ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & eventFilter .~ ef
        & dialog .~ Nothing
        & receivedEvents .~ []
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env (model ^. currentSub)
      , Task $ subscribe env ef
      ]
      where
        pk = deriveXOnlyPubKey k
        ks = Keys k pk True ""
        ef = eventFilterFromKeys ks (model ^. AppTypes.followers)
        dk = disableKeys $ model ^. keys
    ImportSecKey ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & mySecKeyInput .~ ""
        & eventFilter .~ ef
        & dialog .~ Nothing
        & receivedEvents .~ []
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env (model ^. currentSub)
      , Task $ subscribe env (model ^. eventFilter)
      ]
      where
        kp =
          fromJust $
          fmap keyPairFromSecKey $
          maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
        pk = deriveXOnlyPubKey $ kp
        ks = Keys kp pk True ""
        ef = eventFilterFromKeys ks (model ^. AppTypes.followers)
        dk = disableKeys $ model ^. keys
    NoKeysFound ->
      [ Model $ model & dialog .~ Just GenerateKeyPairDialog ]
    ErrorReadingKeysFile ->
      [ Model $ model & dialog .~ Just ErrorReadingKeysFileDialog ]
    SendPost ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    ViewPostDetails re ->
      [ Model $ model & currentView .~ PostDetailsView re ]
    ViewProfile ->
      [ Model $ model
        & currentView .~ ProfileView
        & profileModel . inputs . nameInput .~ name
        & profileModel . inputs . aboutInput .~ about
        & profileModel . inputs . pictureUrlInput .~ pictureUrl
        & profileModel . inputs . nip05IdentifierInput .~ nip05Identifier
      ] where
        (Keys _ xo _ _) = fromJust (model ^. selectedKeys)
        profileData = profileDataFromReceivedEvents
          (model ^. receivedEvents)
          xo
        name = maybe "" pdName profileData
        about = maybe "" pdAbout profileData
        pictureUrl = maybe "" pdPictureUrl profileData
        nip05Identifier = maybe "" pdNip05 profileData
    Back ->
      [ Model $ model
        & currentView .~ PostsView
        & dialog .~ Nothing
      ]
    PostSent -> [ Model $ model & newPostInput .~ "" ]
    ReplyToPost e ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    EventAppeared e r ->
      [ Model $ model & receivedEvents .~ addReceivedEvent (model ^. receivedEvents) e r ]
    CloseDialog ->
      [ Model $ model & dialog .~ Nothing ]

addReceivedEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addReceivedEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r' `sameRelay` r) rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst a) (created_at $ fst b)

subscribe :: AppEnv -> Maybe EventFilter -> IO AppEvent
subscribe env mfilter = do
  case mfilter of
    Just f -> do
      subId <- genSubscriptionId
      atomically $ writeTChan (env ^. channel) $ RequestRelay subId f
      return $ Subscribed subId
    _ ->
      return NoOp

unsubscribe :: AppEnv -> Text -> IO AppEvent
unsubscribe env subId = do
  atomically $ writeTChan (env ^. channel) $ Close subId
  return NoOp

handleNewPost :: AppEnv -> AppModel -> IO AppEvent
handleNewPost env model = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = fromJust $ model ^. selectedKeys
  let raw = case model ^. currentView of {
    PostDetailsView re ->
      replyNote (fst re) (strip $ model ^. newPostInput) xo now;
    _ ->
      textNote (strip $ model ^. newPostInput) xo now;
  }
  atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent raw kp xo
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
      Just k  -> do
        sendMsg $ KeyPairsLoaded k
      _       -> do
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
          _ ->
            WS.sendTextData conn $ encode msg'

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks

switchEnabledKeys :: Keys -> [Keys] -> [Keys]
switchEnabledKeys (Keys kp _ _ _) ks = map (\(Keys kp' xo' a' n') -> if kp == kp'
    then Keys kp' xo' True n'
    else Keys kp' xo' False n'
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
      -- , appDisableAutoScale True
      ]



-- main = do
--     let e = "{\"id\":\"000c76d2d5b9362d962d8ee597460aae9dea1f57673cc7b8b8a358e5b890c8b7\",\"pubkey\":\"22e804d26ed16b68db5259e78449e96dab5d464c8f470bda3eb1a70467f2c793\",\"created_at\":1652805400,\"kind\":3,\"tags\":[[\"p\",\"1d643df20fb811b33c3cfb04c72db04c6ae031dbf68f613ba45407cbdd9446b6\"],[\"p\",\"c2bb5d6529095edbfbdbe3f136175c146c6706526325b32da881c7c34c7b1ab8\"],[\"p\",\"2508ed2c2ab3f6728a880fafbc0895a2afeacbb74eb69847255fb60564af0d85\"],[\"p\",\"aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8\"],[\"p\",\"35d26e4690cbe1a898af61cc3515661eb5fa763b57bd0b42e45099c8b32fd50f\"],[\"p\",\"ad5aab5be883a571ea37b231cd996d37522e77d0f121cedfd6787b91d848268e\"],[\"p\",\"7e88f589d2677ea4a863c72af5d0e85fbe1d3db111667c50d33fa42196a1afc0\"],[\"p\",\"904ea00a4a245559d6184be5c6e2cf2c66ea7fc91eb5f1eb5349506d19d63a11\"],[\"p\",\"2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5\"],[\"p\",\"b3349a7b2ac10694a208a2976710379a53d279a77bf4200d5e3ce6f9ac92fcd3\"],[\"p\",\"95405f16211a88c869ec87b684cb450136b7bf2420e236f9ec793385893d01e8\"],[\"p\",\"06fca9f06f74cf86a16fe4c2feec508700643e2b105b519fd93d35332c51ad53\"],[\"p\",\"e9e4276490374a0daf7759fd5f475deff6ffb9b0fc5fa98c902b5f4b2fe3bba2\"],[\"p\",\"1fe3ade4cf9f6a8e05e4820e28eae5ff2b7188f6f042740db57a31f6b041555f\"],[\"p\",\"495fab2968e4ed154414702b72255c25957786fbd578736edb9ebd34386abdc2\"],[\"p\",\"3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d\"],[\"p\",\"fd3fdb0d0d8d6f9a7667b53211de8ae3c5246b79bdaf64ebac849d5148b5615f\"],[\"p\",\"bebe06cf128be9b207eae489348693775299c5032fbb17cf12dbaff88c519c82\"],[\"p\",\"547fcc5c7e655fe7c83da5a812e6332f0a4779c87bf540d8e75a4edbbf36fe4a\"],[\"p\",\"75fc5ac2487363293bd27fb0d14fb966477d0f1dbc6361d37806a6a740eda91e\"],[\"p\",\"29ec4adf101d4113e0630aa7c2d2e9f00b14fcbf583fb14d3126694e31d03eeb\"],[\"p\",\"b2d670de53b27691c0c3400225b65c35a26d06093bcc41f48ffc71e0907f9d4a\"],[\"p\",\"bc6f08b3630d1afbef42db40c6650ebc666de01312ddfe7c6443dc23fe9905c4\"],[\"p\",\"05fecd5f87ae5a94aeaf15e3e5f03c9ef7bb1e284a9edc67740f48a4643f589b\"],[\"p\",\"41108126409bf99cb77ff16fd53f4da2e53010b0dca04b0a53ebdf46eade37aa\"],[\"p\",\"ed1d0e1f743a7d19aa2dfb0162df73bacdbc699f67cc55bb91a98c35f7deac69\"],[\"p\",\"b44b0193849f60587f0a17cccd3ade26668a7e36eb453acb1a94e6c1f45b4af3\"],[\"p\",\"6b0d4c8d9dc59e110d380b0429a02891f1341a0fa2ba1b1cf83a3db4d47e3964\"],[\"p\",\"f6b7d47f9ef5e580424359b57dbcabeb1af7e9a01994d755db67f330d927651f\"],[\"p\",\"3167ac15eaf57df64f51fee97caa7383a3d668da81144408c4af60f1c0316450\"],[\"p\",\"356e99a0f75e973c0512873cbdce0385df39712653020af825556ceb4afc3ba8\"],[\"p\",\"2a308bcb53ae7c35e59c2d76eb11dcf93bc2106e02a4fffbd10bb817ea644d01\"],[\"p\",\"50c603ef273f6dd1cdd7c4a89dd59279b917e073b5d982d8b2f18845cf19d769\"],[\"p\",\"168b5acb915b1ea5bcdefb460823fd9cd56399da75a8da78459fa1a1bdc7df32\"],[\"p\",\"f0bed2e11260f0f77f781db928f40a34c18713fda1918d3be996f91d0776e985\"],[\"p\",\"2a80a638193299b36eca86edc3952e70555eef45c78d9349c63fd71344923f1e\"],[\"p\",\"ea75802dd1c86933c1e20c582541bb283d44c88e3445ed90d4375fc3d973f3a0\"],[\"p\",\"577e3b24dc1de957ad12b042386cbe4a229be07114ef1f61bd5aeaa9d9e86606\"],[\"p\",\"eb40d75f691b940fd206c639ab8031c16d7d0d3eb08af57423354e6b0f128588\"],[\"p\",\"8f6ddf42ebe9486308975de3f27cc783bed2d93d1f7b0804d5e7090c5fad93b6\"],[\"p\",\"827ebfd1330ba0ec23f4da882dcf74abae85337f5213025cc0b7c916bb1abbd5\",\"wss://rsslay.fiatjaf.com\"],[\"p\",\"b0b340bad0e3fe2cc29a286664134bddfcf89be3b11c8684f44d6b9e685d4e15\",\"wss://rsslay.fiatjaf.com\"],[\"p\",\"abf12c3fad52ab1a5313fea429a5021ecb652b4e801d516c1c1f203f0c575983\",\"wss://nostr-pub.wellorder.net\"],[\"p\",\"4e9069816400fe14fde2566a0a620e9c421a6f5ec459ae05b5bc5f8870d0fbd6\",\"wss://nostr-relay.untethr.me\"],[\"p\",\"1bbb8324577ac089607e45813bac499ebdab4621d029f8c02b2c82b4410fd3f4\"],[\"p\",\"d90a37f79f7d7339006e1bc4ac2090c9f8867c79bdfafefc75e780e04f3e8777\"],[\"p\",\"b0635d6a9851d3aed0cd6c495b282167acf761729078d975fc341b22650b07b9\"],[\"p\",\"bc6403e08c21c3ebe3e283a1de1b5517fe7502ec47991bab350a05f5b1a5c235\"],[\"p\",\"4fecd36ea23ba215c56ec3a5f26b4eed8e01c8d2c5a4700d2222393677f2d39f\"],[\"p\",\"776bc89bf77bd86df56ed638d323bb2d8c6857d67e09d17c7649b07d9f8bf0fd\",\"wss://rsslay.fiatjaf.com\"],[\"p\",\"b4714a93ef57b27fdb05d1fe02c8392aa847bc431740843be0092e963116433b\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"942d2eac93d2b30d9941a63c42d22e9d1f8cb6b538bb7030391bd886a17264ff\",\"wss://rsslay.fiatjaf.com\"],[\"p\",\"32e1827635450ebb3c5a7d12c1f8e7b2b514439ac10a67eef3d9fd9c5c68e245\",\"wss://nostr-pub.wellorder.net\"],[\"p\",\"6cad545430904b84a8101c5783b65043f19ae29d2da1076b8fc3e64892736f03\"],[\"p\",\"c35809ce922d2883a12cb7fe1826237c268556549b2f3ff1166e1fcbba1f7592\"],[\"p\",\"fc6cddcc4dceeec8f72cf6ad73593cd881281eff985a712332e57d66b1a1f3ea\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"e8b487c079b0f67c695ae6c4c2552a47f38adfa2533cc5926bd2c102942fdcb7\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"8ba2a6b558eeb7fccd1862b905ae9d9408cfbc208f1680d1262733246e92d4da\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"d7b76d02c758a62a505e03bd5f5049aaee4e7e36283d273c7f6798912692df2b\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"f6aba1ce9d584fc6bd91e3107366632cdca37eea2572aba76d8eadfdb218b0bd\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"a840152f7d8ce6c8a1c9abbd848272515d89acd584f860f37a1617fdb5a9fc1e\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"b34417513f66497d7b0e1a8406b6689ac32afb184027717e57d281ea19186315\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"ea543c10c5d7b7730934ded8f93f5b2ec3ea329e9aed71526c5d388f7c00cdc7\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"27da3f032e0fea007947b0da12f1183630c5a2da79d7202b96f35f16ef6ce48e\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"04c915daefee38317fa734444acee390a8269fe5810b2241e5e6dd343dfbecc9\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"33b39db7c93f5799d1adcec5b9c299867e747344ebb849cc21ec91be66d132f9\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"9ec7a778167afb1d30c4833de9322da0c08ba71a69e1911d5578d3144bb56437\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"46fcbe3065eaf1ae7811465924e48923363ff3f526bd6f73d7c184b16bd8ce4d\",\"wss://nostr-relay.wlvs.space\"],[\"p\",\"8c0da4862130283ff9e67d889df264177a508974e2feb96de139804ea66d6168\",\"wss://nostr-relay.wlvs.space\"],[\"nonce\",\"212\"]],\"content\":\"{\\\"wss://nostr-pub.wellorder.net\\\":{\\\"read\\\":true,\\\"write\\\":true},\\\"wss://nostr-relay.wlvs.space\\\":{\\\"read\\\":true,\\\"write\\\":true},\\\"wss://rsslay.fiatjaf.com\\\":{\\\"read\\\":true,\\\"write\\\":false}}\",\"sig\":\"dfb6335e79d9c135ffd793f6416a4af82fdbb68131a21546bd12a3d2d4c90dc88ee619ed0b4b1231525a6371c839de060a007d5d6d094f8cd9e5a75fe90691c5\"}";
--     -- let d = parseE e
--     let d = parseE e
--     putStrLn $ show d
--
-- parseE :: LazyBytes.ByteString -> Either String Event
-- parseE = eitherDecode


-- main = do
--     let e = "\"c35809ce922d2883a12cb7fe1826237c268556549b2f3ff1166e1fcbba1f7592\"";
--     -- let d = parseE e
--     let d = parseE e
--     putStrLn $ show d
--
-- parseE :: LazyBytes.ByteString -> Either String XOnlyPubKey
-- parseE = eitherDecode



{- for debugging json parsers
main = do
    let e = "[\"EVENT\",\"044039d07ff47f100e29debaec66a3cd35e02b0cb849a3bce2cfd8bc0a1629f1\",{\"id\":\"763500a66f5ed2cc3bf77879082dc406a119ad177d12c2727e671a397a60fcfb\"
,\"pubkey\":\"1702e3b17de25d9fd63d80fb1a2394a26239cb2a747b893e82f776704d888c4b\",\"created_at\":1649731192,\"kind\":1,\"tags\":[[\"e\",\"d87c30cd198635dad4e6981b907fa4ea2
608a6e675844ec798f85ca6bafa2a34\",\"\"]],\"content\":\"ff\",\"sig\":\"54436b7cd3afbb5c9bb7e3da455725a27c8f064acc52febe9d031918d07135acbd001192751e19579ec7cdbe76ff36631e26
f02f25da94a5c5a9cc6f2ffe97c8\"}]\n";
    let d = parseE e
    putStrLn $ show d

parseE :: LazyBytes.ByteString -> Either String ServerResponse
parseE = eitherDecode
-}
