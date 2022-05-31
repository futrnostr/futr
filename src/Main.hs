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
import           Crypto.Schnorr                       (XOnlyPubKey, KeyPair,
                                                       decodeHex, deriveXOnlyPubKey,
                                                       keyPairFromSecKey, generateKeyPair,
                                                       secKey, xOnlyPubKey)
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base16               as B16
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.DateTime
import           Data.Default
import           Data.Either                          (fromRight)
import           Data.List                            (find, sort, sortBy)
import qualified Data.Map                             as Map
import           Data.Maybe
import           Data.Monoid                          (mconcat)
import           Data.Text                            (Text, strip)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Monomer
import           Monomer.Widgets.Single
import qualified Network.Connection                   as Connection
import qualified Network.HTTP.Req                     as Req
import           Network.Socket
import           Network.WebSockets                   (ClientApp, Connection,
                                                       receiveData, sendClose,
                                                       sendTextData)
import qualified Network.WebSockets                   as WS
import qualified Network.WebSockets.Stream            as Stream
import           System.Directory                     (doesFileExist)
import qualified Text.URI                             as URI
import           Text.URI.Lens
import           Wuss

import           AppTypes
import           Helpers
import           NostrFunctions
import           Nostr.Event
import           Nostr.Filter
import           Nostr.Keys
import           Nostr.Profile
import           Nostr.Relay
import           Nostr.Request  (Request(..), Subscription(..))
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
            (AppTypes.Subscription subId _) = model ^. currentSub
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
      , Task $ subscribe env (model ^. eventFilters)
      ] where
          newPool = r : (poolWithoutRelay (model ^. pool) r)
    RelayDisconnected r ->
      [ Model $ model & pool .~ newPool ]
      where
        newPool = (r {connected = False}) : (poolWithoutRelay (model ^. pool) r)
    ValidateAndAddRelay ->
      [ Task $ validateAndAddRelay (model ^. relayModel) ]
    InvalidRelayURI ->
      [ Model $ model & relayModel . isInvalidInput .~ True ]
    AddRelay r ->
      [ Producer $ connectRelay env r
      , Model $ model
          & dialog .~ Nothing
          & relayModel . relayURI .~ "wss://"
          & relayModel . relayReadableInput .~ True
          & relayModel . relayWritableInput .~ True
          & pool .~ r : (poolWithoutRelay (model ^. pool) r)
      ]
    ShowDialog d ->
      case d of
        RelayDialog r ->
          [ Model $ model
            & dialog .~ Just (RelayDialog r)
            & relayModel . relayReadableInput .~ readable info'
            & relayModel . relayWritableInput .~ writable info'
          ]
          where
            info' = info r
        NewRelayDialog ->
          [ Model $ model & dialog .~ Just NewRelayDialog ]
        GenerateKeyPairDialog ->
          [ Model $ model & dialog .~ Just GenerateKeyPairDialog ]
        DeleteEventDialog event ->
          [ Model $ model & dialog .~ (Just $ DeleteEventDialog event) ]
        _ -> []
    AppTypes.Subscribe fs ->
      [ Model $ model & eventFilters .~ fs
      , Task $ subscribe env fs
      ]
    Subscribed subId t ->
      [ Model $ model & currentSub .~ AppTypes.Subscription subId t ]
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
        (AppTypes.Subscription subId _) = model ^. currentSub
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
        (AppTypes.Subscription subId _) = model ^. currentSub
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
        (AppTypes.Subscription subId _) = model ^. currentSub
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
    1 -> -- text notes
      model
        & currentSub .~ (AppTypes.Subscription subId $ created_at e)
        & viewPostsModel . ViewPosts.receivedEvents .~
            addReceivedEvent (model ^. viewPostsModel . ViewPosts.receivedEvents) e r
        & viewProfileModel . ViewProfile.viewPostsModel . ViewPosts.receivedEvents .~
            addReceivedEvent (model ^. viewPostsModel . ViewPosts.receivedEvents) e r
      where
        (AppTypes.Subscription subId _) = model ^. currentSub
    0 -> -- set metadata
      model
        & keys .~ map (\ks -> updateName ks) (model ^. keys)
        & selectedKeys .~ fmap (\ks -> updateName ks) (model ^. selectedKeys)
        & profiles .~ addProfile (model ^. profiles) e r
        & currentSub .~ (AppTypes.Subscription subId $ created_at e)
        & viewPostsModel . ViewPosts.profiles .~
            addProfile (model ^. profiles) e r
        & viewProfileModel . ViewProfile.viewPostsModel . ViewPosts.profiles .~
            addProfile (model ^. profiles) e r
      where
        mp = decode $ LazyBytes.fromStrict $ encodeUtf8 $ content e
        (AppTypes.Subscription subId _) = model ^. currentSub
        name = maybe "" name mp
        xo' = pubKey e
        updateName (Keys kp xo a n) = if xo == xo'
          then Keys kp xo a (Just name)
          else Keys kp xo a n
    3 -> -- contact list
      model
        & following .~
            addFollowing (model ^. following) e
        & viewProfileModel . ViewProfile.following .~
            addFollowing (model ^. following) e
        & currentSub .~ (AppTypes.Subscription subId $ created_at e)
      where
        (AppTypes.Subscription subId _) = model ^. currentSub
    5 -> trace "delete event" model
    _ ->
      model

addReceivedEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addReceivedEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r' == r) rs)
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
    xo = pubKey e
    u = relayName r

addFollowing :: Map.Map XOnlyPubKey [Profile] -> Event -> Map.Map XOnlyPubKey [Profile]
addFollowing pm e =
  Map.insert xo (tagsToProfiles tags') pm
  where
    tags' = tags e
    xo = pubKey e

buildEventFilters :: XOnlyPubKey -> Map.Map XOnlyPubKey [Profile] -> Maybe DateTime -> IO AppEvent
buildEventFilters xo pm latest = do
  recent <- last24h
  let ps = Map.findWithDefault [] xo pm
  return $ AppTypes.Subscribe
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

subscribe :: AppEnv -> [Filter] -> IO AppEvent
subscribe env [] = return NoOp
subscribe env fs = do
  now <- getCurrentTime
  subId <- genSubscriptionId
  atomically $ writeTChan (env ^. channel) $ Nostr.Request.Subscribe fs subId
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

extraSubscribe :: AppEnv -> [Filter] -> IO AppEvent
extraSubscribe env [] = return NoOp
extraSubscribe env fs = do
  now <- getCurrentTime
  subId <- genSubscriptionId
  let sub = Nostr.Request.Subscription fs subId
  atomically $ writeTChan (env ^. channel) $ Nostr.Request.Subscribe sub
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

validateAndAddRelay :: RelayModel -> IO AppEvent
validateAndAddRelay model = do
  uri <- URI.mkURI $ model ^. relayURI
  return $ if isValidRelayURI uri
    then do
      let info = RelayInfo (model ^. relayReadableInput) (model ^. relayWritableInput)
      AddRelay (Relay uri info False)
    else
      InvalidRelayURI

connectRelay :: AppEnv -> Relay -> (AppEvent -> IO ()) -> IO ()
connectRelay env r sendMsg = if connected r then return () else do
  putStrLn $ "trying to connect to " ++ (T.unpack $ relayName r) ++ " ..."
  start $ \conn -> do
    let r' = r { connected = True }
    putStrLn $ "Connected to " ++ (T.unpack $ relayName r)
    sendMsg $ RelayConnected r'
    if readable info' then receiveWs r' conn sendMsg else sendMsg NoOp
    if writable info' then sendWs (env ^. channel) r' conn sendMsg else sendMsg NoOp
  where
    host = T.unpack $ extractHostname r
    port = extractPort r
    path = T.unpack $ extractPath r
    info' = info r
    start = case extractScheme r of
      "wss" -> runSecureClient host (fromIntegral port) path
      "ws"  -> WS.runClient host port path

disconnectRelay :: AppEnv -> Relay -> IO AppEvent
disconnectRelay env r = if not $ connected r then return NoOp else do
  atomically $ writeTChan (env ^. channel) $ Disconnect r
  return NoOp

receiveWs :: Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
receiveWs r conn sendMsg = void . forkIO $ void . runMaybeT $ forever $ do
  msg <- lift (Exception.try $ WS.receiveData conn :: IO (Either WS.ConnectionException LazyBytes.ByteString))
  case msg of
    Left ex    -> do
      liftIO $ putStrLn $ "Connection to " ++ (T.unpack $ relayName r) ++ " closed"
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

sendWs :: TChan Request -> Relay -> WS.Connection -> (AppEvent -> IO ()) -> IO ()
sendWs broadcastChannel r conn sendMsg = do
  channel <- atomically $ dupTChan broadcastChannel
  forever $ do
    msg <- Exception.try $ liftIO . atomically $ readTChan channel :: IO (Either WS.ConnectionException Request)
    case msg of
      Left ex -> sendMsg $ RelayDisconnected r
      Right msg' -> do
        case msg' of
          Disconnect r' ->
            if r' == r then do
                WS.sendClose conn $ T.pack "Bye!"
            else return ()
          _ ->
            WS.sendTextData conn $ encode msg'

generateNewKeyPair :: (AppEvent -> IO ()) -> IO ()
generateNewKeyPair sendMsg = do
  k <- generateKeyPair
  sendMsg $ KeyPairGenerated k

switchEnabledKeys :: Keys -> [Keys] -> [Keys]
switchEnabledKeys (Keys kp _ _ _) ks =
  map (\(Keys kp' xo' a' n') -> if kp == kp'
    then Keys kp' xo' True n'
    else Keys kp' xo' False n'
  ) ks

errorReadingKeysFileStack :: SetupNode
errorReadingKeysFileStack =
  vstack
    [ label "ERROR: Keys file could not be read." ]

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
