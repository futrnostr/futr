{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Concurrent                   (forkIO, threadDelay)
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
import           Data.Text.Encoding                   (encodeUtf8)
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
import           UIHelpers
import           Widgets.EditProfile
import qualified Widgets.ViewProfile                  as ViewProfile
import qualified Widgets.ViewPosts                    as ViewPosts

import           Debug.Trace as Trace
import qualified Crypto.Hash.SHA256     as SHA256 -- debug only

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
            & viewPostsModel .~ def
            & currentView .~ PostsView
            & searchInput .~ ""
          , Task $ saveKeyPairs keys'
          , Task $ unsubscribe env subId
          , Task $ buildEventFilters xo (model ^. following) Nothing
          ] where
            keys' = switchEnabledKeys ks (model ^. keys)
            (Keys _ xo _ _) = ks
            (Subscription subId _) = model ^. currentSub
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
      , Producer timerLoop
      ] ++ (map (\r -> Producer $ connectRelay env r) (model ^. pool) )
    RelayConnected r ->
      [ Model $ model & pool .~ newPool
      , Task $ subscribe env (model ^. eventFilters)
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
        DeleteEventDialog event ->
          [ Model $ model & dialog .~ (Just $ DeleteEventDialog event) ]
        _ -> []
    Subscribe fs ->
      [ Model $ model & eventFilters .~ fs
      , Task $ subscribe env fs
      ]
    Subscribed subId t ->
      [ Model $ model & currentSub .~ Subscription subId t ]
    ExtraSubscribe fs ->
      [ Model $ model & extraFilters .~ fs
      , Task $ extraSubscribe env fs
      ]
    ExtraSubscribed subId ->
      [ Model $ model & extraSub .~ subId ]
    KeyPairsLoaded ks ->
      [ Model $ model
        & keys .~ ks
        & selectedKeys .~ Just mk
        & dialog .~ Nothing
        & viewPostsModel .~ def
        & following .~ newFollowing
      , Task $ buildEventFilters xo (model ^. following) Nothing
      ]
      where
        mk = mainKeys ks
        (Keys _ xo _ _) = mk
        newFollowing = Map.insert xo [] (model ^. following)
    GenerateKeyPair ->
      [ Producer generateNewKeyPair ]
    KeyPairGenerated k ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & dialog .~ Nothing
        & viewPostsModel .~ def
        & following .~ newFollowing
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env subId
      , Task $ buildEventFilters xo (model ^. following) Nothing
      ]
      where
        xo = deriveXOnlyPubKey k
        ks = Keys k xo True Nothing
        dk = disableKeys $ model ^. keys
        newFollowing = Map.insert xo [] (model ^. following)
        (Subscription subId _) = model ^. currentSub
    ImportSecKey ->
      [ Model $ model
        & keys .~ ks : dk
        & selectedKeys .~ Just ks
        & mySecKeyInput .~ ""
        & dialog .~ Nothing
        & viewPostsModel .~ def
        & following .~ newFollowing
      , Task $ saveKeyPairs $ ks : dk
      , Task $ unsubscribe env subId
      , Task $ buildEventFilters xo (model ^. following) Nothing
      ]
      where
        kp =
          fromJust $
          fmap keyPairFromSecKey $
          maybe Nothing secKey $ decodeHex $ model ^. mySecKeyInput
        xo = deriveXOnlyPubKey $ kp
        ks = Keys kp xo True Nothing
        dk = disableKeys $ model ^. keys
        newFollowing = Map.insert xo [] (model ^. following)
        (Subscription subId _) = model ^. currentSub
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
    EditProfile ->
      [ Model $ model
        & currentView .~ EditProfileView
        & editProfileModel . inputs . nameInput .~ name
        & editProfileModel . inputs . aboutInput .~ about
        & editProfileModel . inputs . pictureUrlInput .~ pictureUrl
        & editProfileModel . inputs . nip05IdentifierInput .~ nip05Identifier
      ] where
        (Keys _ xo _ _) = fromJust (model ^. selectedKeys)
        (ProfileData name about pictureUrl nip05Identifier) = case Map.lookup xo (model ^. AppTypes.profiles) of
          Just (Profile _ _ pd) -> pd
          Nothing -> def
    ViewProfile xo ->
      [ Model $ model
        & currentView .~ ProfileView xo
        & viewProfileModel .~ vpm
      , Task $ buildProfileEventFilters xo
      ] where
        (ProfileData name about pictureUrl nip05Identifier) = case Map.lookup xo (model ^. AppTypes.profiles) of
          Just (Profile _ _ pd) -> pd
          Nothing -> def
        vpm = (model ^. viewProfileModel)
          { ViewProfile._myKeys = model ^. selectedKeys
          , ViewProfile._name = name
          , ViewProfile._about = about
          , ViewProfile._pictureUrl = pictureUrl
          , ViewProfile._nip05Identifier = nip05Identifier
          , ViewProfile._following = model ^. following
          , ViewProfile._xo = Just xo
          , ViewProfile._viewPostsModel = model ^. viewPostsModel
          }
    SearchProfile v ->
      [ Task $ runSearchProfile v
      ]
    DeleteEvent e ->
      [ Model $ model
          & deleteReason .~ defaultDeleteReason
      , Task $ handleDeleteEvent env e model
      ]
    Back ->
      [ Model $ model
        & currentView .~ PostsView
        & dialog .~ Nothing
        & extraSub .~ ""
        & extraFilters .~ []
      , Task $ unsubscribe env $ model ^. extraSub
      ]
    EventSent ->
      [ Model $ model
          & dialog .~ Nothing
      ]
    ReplyToPost e ->
      [ Model $ model
          & newPostInput .~ ""
      , Task $ handleNewPost env model
      ]
    EventAppeared e r ->
      updateModel ++ resubscribe
      where
        (Keys _ xo _ _) = fromJust $ model ^. selectedKeys
        newModel = handleReceivedEvent model e r
        (Subscription subId _) = model ^. currentSub
        updateModel =
          [ Model $ newModel
          , Task $ maybeSaveKeyPairs model newModel
          ]
        resubscribe = case kind e of
          3 ->
            [ Task $ unsubscribe env subId
            , Task $ buildEventFilters xo (newModel ^. following) (Just $ created_at e)
            ]
          _ ->
            []
    CloseDialog ->
      [ Model $ model & dialog .~ Nothing ]
    TimerTick now ->
      [ Model $ model & AppTypes.time .~ now ]

handleReceivedEvent :: AppModel -> Event -> Relay -> AppModel
handleReceivedEvent model e r =
  case kind (trace (show e) e) of
    1 ->
      model
        & currentSub .~ (Subscription subId $ created_at e)
        & viewPostsModel . ViewPosts.receivedEvents .~
            addReceivedEvent (model ^. viewPostsModel . ViewPosts.receivedEvents) e r
        & viewProfileModel . ViewProfile.viewPostsModel . ViewPosts.receivedEvents .~
            addReceivedEvent (model ^. viewPostsModel . ViewPosts.receivedEvents) e r
      where
        (Subscription subId _) = model ^. currentSub
    0 ->
      model
        & keys .~ map (\ks -> updateName ks) (model ^. keys)
        & selectedKeys .~ fmap (\ks -> updateName ks) (model ^. selectedKeys)
        & profiles .~ addProfile (model ^. profiles) e r
        & currentSub .~ (Subscription subId $ created_at e)
        & viewPostsModel . ViewPosts.profiles .~
            addProfile (model ^. profiles) e r
        & viewProfileModel . ViewProfile.viewPostsModel . ViewPosts.profiles .~
            addProfile (model ^. profiles) e r
      where
        mp = decode $ LazyBytes.fromStrict $ encodeUtf8 $ content e
        (Subscription subId _) = model ^. currentSub
        name = maybe "" pdName mp
        xo' = NT.pubKey e
        updateName (Keys kp xo a n) = if xo == xo'
          then Keys kp xo a (Just name)
          else Keys kp xo a n
    3 ->
      model
        & following .~
            addFollowing (model ^. following) e
        & viewProfileModel . ViewProfile.following .~
            addFollowing (model ^. following) e
        & currentSub .~ (Subscription subId $ created_at e)
      where
        (Subscription subId _) = model ^. currentSub
    5 -> trace "delete event" model
    _ ->
      model

addReceivedEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addReceivedEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r' `sameRelay` r) rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)

addProfile :: Map.Map XOnlyPubKey Profile -> Event -> Relay -> Map.Map XOnlyPubKey Profile
addProfile profiles e r =
  case decode $ LazyBytes.fromStrict $ encodeUtf8 $ content e of
    Just pd ->
      Map.insert xo (Profile xo u pd) profiles
    Nothing ->
      profiles
  where
    xo = NT.pubKey e
    u = T.pack $ relayName r

addFollowing :: Map.Map XOnlyPubKey [Profile] -> Event -> Map.Map XOnlyPubKey [Profile]
addFollowing pm e =
  Map.insert xo (tagsToProfiles tags') pm
  where
    tags' = tags e
    xo = NT.pubKey e

buildEventFilters :: XOnlyPubKey -> Map.Map XOnlyPubKey [Profile] -> Maybe DateTime -> IO AppEvent
buildEventFilters xo pm latest = do
  recent <- last24h
  let ps = Map.findWithDefault [] xo pm
  return $ Subscribe
    [ AllProfilesFilter latest
    , OwnEventsFilter xo $ fromDate recent
    , MentionsFilter xo $ fromDate recent
    , FollowersFilter ps $ fromDate recent
    ]
  where
    fromDate recent = case latest of
      Just t -> t
      Nothing -> recent

buildProfileEventFilters :: XOnlyPubKey -> IO AppEvent
buildProfileEventFilters xo = do
  recent <- last24h
  return $ ExtraSubscribe
    [ OwnEventsFilter xo recent
    , ProfileFollowers xo
    ]

last24h :: IO DateTime
last24h = do
  now <- getCurrentTime
  return $ fromSeconds $ toSeconds now - 86400

subscribe :: AppEnv -> [EventFilter] -> IO AppEvent
subscribe env [] = return NoOp
subscribe env fs = do
  now <- getCurrentTime
  subId <- genSubscriptionId
  atomically $ writeTChan (env ^. channel) $ RequestRelay subId fs
  return $ Subscribed subId now

unsubscribe :: AppEnv -> Text -> IO AppEvent
unsubscribe env subId = do
  atomically $ writeTChan (env ^. channel) $ Close subId
  return NoOp

runSearchProfile :: Text -> IO AppEvent
runSearchProfile v = do
  case maybe Nothing xOnlyPubKey $ decodeHex v of
    Just xo ->
      return $ ViewProfile xo
    _ ->
      return NoOp

extraSubscribe :: AppEnv -> [EventFilter] -> IO AppEvent
extraSubscribe env [] = return NoOp
extraSubscribe env fs = do
  now <- getCurrentTime
  subId <- genSubscriptionId
  atomically $ writeTChan (env ^. channel) $ RequestRelay subId fs
  return $ ExtraSubscribed subId

handleNewPost :: AppEnv -> AppModel -> IO AppEvent
handleNewPost env model = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = fromJust $ model ^. selectedKeys
  let unsigned = case model ^. currentView of {
    PostDetailsView re ->
      replyNote (fst re) (strip $ model ^. newPostInput) xo now;
    _ ->
      textNote (strip $ model ^. newPostInput) xo now;
  }
  atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent unsigned kp xo
  return EventSent

handleDeleteEvent :: AppEnv -> Event -> AppModel -> IO AppEvent
handleDeleteEvent env event model = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = fromJust $ model ^. selectedKeys
  let unsigned = deleteEvents [eventId event] (model ^. deleteReason) xo now
  atomically $ writeTChan (env ^. channel) $ SendEvent $ signEvent unsigned kp xo
  return EventSent

maybeSaveKeyPairs :: AppModel -> AppModel -> IO AppEvent
maybeSaveKeyPairs old new =
  if (old ^. keys) /= (new ^. keys)
    then saveKeyPairs $ new ^. keys
    else return NoOp

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
        -- todo: handle notices
        -- lift $ putStrLn $ show $ extractEventFromServerResponse m
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

timerLoop :: (AppEvent -> IO ()) -> IO ()
timerLoop sendMsg = void . forkIO $ void $ forever $ do
  now <- getCurrentTime
  sendMsg $ TimerTick now
  threadDelay 5000000

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks

switchEnabledKeys :: Keys -> [Keys] -> [Keys]
switchEnabledKeys (Keys kp _ _ _) ks =
  map (\(Keys kp' xo' a' n') -> if kp == kp'
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
      , appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf"
      , appInitEvent AppInit
      -- , appDisableAutoScale True
      ]



-- main = do
--      let e = "{\"id\":\"a948e858fcdb8c8e5dcc88c387cc37f27872fad63473bb8d2ea77745bac61e39\",\"pubkey\":\"a5b365b10946f00071d56c50f86ab2e36d3e95406f04135814969c59fb0e7800\",\"created_at\":1653201423,\"kind\":3,\"tags\":[[\"p\",\"a5b365b10946f00071d56c50f86ab2e36d3e95406f04135814969c59fb0e7800\",\"\",\"oliverP\"]],\"content\":\"\",\"sig\":\"f99071b5c8e31f202816da7738421cb3ef4719484c659a54974f8bbc73061637db4b9235d3676720a384b32a3e57bd9756e2d35db6a1857b29b55872081dec92\"}";
-- --     -- let d = parseE e
--      let d = parseE e
--      putStrLn $ show d
-- --
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
