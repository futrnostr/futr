{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Home where

import Control.Concurrent           (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad                (forever, mzero, void)
import Control.Monad.STM            (atomically)
import Crypto.Schnorr               (XOnlyPubKey)
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List                    (find, sortBy)
import Data.Map (Map)
import Data.Maybe
import Data.Text                    (Text, strip)
import Data.Text.Encoding           (encodeUtf8)
import Monomer

import qualified Data.ByteString.Lazy as LazyBytes

import Helpers
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Request
import Nostr.Response

import qualified Data.Map as Map
import qualified Widgets.EditProfile  as EditProfile
import qualified Widgets.ViewPosts    as ViewPosts

type HomeWenv = WidgetEnv HomeModel HomeEvent

type HomeNode = WidgetNode HomeModel HomeEvent

data HomeModel = HomeModel
  { _myKeys           :: Keys
  , _profileImage     :: Text
  , _events           :: [ReceivedEvent]
  , _contacts         :: Map XOnlyPubKey (Profile, DateTime)
  , _noteInput        :: Text
  , _homeSub          :: SubscriptionId
  , _initSub          :: SubscriptionId
  , _viewPostsModel   :: ViewPosts.ViewPostsModel
  } deriving (Eq, Show)

instance Default HomeModel where
  def = HomeModel initialKeys "" [] Map.empty "" "" "" def

data HomeEvent
  -- subscriptions
  = Initialize
  | Initialized (Map XOnlyPubKey (Profile, DateTime))
  | ContactsReceived [(XOnlyPubKey, (Profile, DateTime))]
  | TextNoteReceived Event Relay
  -- actions
  | SendPost
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  deriving Show

makeLenses 'HomeModel

homeWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> MVar RelayPool
  -> ALens' sp HomeModel
  -> WidgetNode sp ep
homeWidget channel pool model = composite_ "HomeWidget" model buildUI (handleHomeEvent channel pool) [ onInit Initialize ]

handleHomeEvent
  :: TChan Request
  -> MVar RelayPool
  -> HomeWenv
  -> HomeNode
  -> HomeModel
  -> HomeEvent
  -> [EventResponse HomeModel HomeEvent sp ep]
handleHomeEvent channel pool env node model evt = case evt of
  -- subscriptions
  Initialize ->
    [ Producer $ loadContacts channel pool (model ^. myKeys) ]
  Initialized cs ->
    [ Model $ model & contacts .~ cs
    , Producer $ initSubscriptions channel pool (model ^. myKeys) (Map.keys cs)
    ]
  ContactsReceived cs ->
    [ Model $ model & contacts .~ updateContacts (model ^. contacts) cs
    ]
  TextNoteReceived event relay ->
    [ Model $ model
        & viewPostsModel . ViewPosts.events .~ newEvents
        & events .~ newEvents
    ]
    where
      newEvents = addEvent (model ^. events) event relay
  SendPost ->
    [ Model $ model
        & noteInput .~ ""
    , Producer $ sendPost channel model
    ]
  ViewPostDetails re ->
    []
  ViewProfile xo ->
    []

updateContacts :: Map XOnlyPubKey (Profile, DateTime) -> [(XOnlyPubKey, (Profile, DateTime))] -> Map XOnlyPubKey (Profile, DateTime)
updateContacts original new =
  Map.union newContacts original
  where
    newContacts = Map.fromList $ catMaybes $ map (checkContactIsNewer original) new

checkContactIsNewer :: Map XOnlyPubKey (Profile, DateTime) -> (XOnlyPubKey, (Profile, DateTime)) -> Maybe (XOnlyPubKey, (Profile, DateTime))
checkContactIsNewer original (xo, (p, d)) =
  case Map.lookup xo original of
    Nothing ->
      Just (xo, (p, d))
    Just (p', d') ->
      if d' > d
        then Just (xo, (p', d'))
        else Nothing

initSubscriptions
  :: TChan Request
  -> MVar RelayPool
  -> Keys
  -> [XOnlyPubKey]
  -> (HomeEvent -> IO ())
  -> IO ()
initSubscriptions request pool (Keys _ xo _ _) contacts sendMsg = do
  response <- atomically newTChan
  subId <- subscribe pool request response initialFilters
  void . forkIO $ void . forever $ do
    msg <- atomically $ readTChan response
    case msg of
      (EventReceived _ event, relay) -> do
        case kind event of
          TextNote -> do
            sendMsg $ TextNoteReceived event relay
          Contacts -> do
            sendMsg $ ContactsReceived $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
          Metadata -> do
            case parseProfiles event of
              Just p -> sendMsg $ ContactsReceived [ p ]
              Nothing -> return ()
          _ -> putStrLn "Unexpected event kind received when loading contacts" -- @todo handle differently

      _ -> mzero
    where
      initialFilters = [ MetadataFilter contacts, TextNoteFilter contacts ]
      parseProfiles e = case readProfile e of
        Just p -> Just (pubKey e, (p, created_at e))
        Nothing -> Nothing

loadContacts
  :: TChan Request
  -> MVar RelayPool
  -> Keys
  -> (HomeEvent -> IO ())
  -> IO ()
loadContacts request pool (Keys _ xo _ _) sendMsg = do
  response <- atomically newTChan
  subId <- subscribe pool request response [ ContactsFilter [ xo ] ]
  msg <- atomically $ readTChan response
  case msg of
    (EventReceived _ event, _) -> do
      case kind event of
        Contacts -> do
          unsubscribe pool request subId
          let contacts = Map.fromList $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
          sendMsg $ Initialized contacts
        _ -> putStrLn "Unexpected event kind received when loading contacts" -- @todo handle differently
    _ -> mzero
  where

tagToProfile :: DateTime -> Tag -> Maybe (XOnlyPubKey, (Profile, DateTime))
tagToProfile datetime (PTag (ValidXOnlyPubKey xo) _ name) = Just (xo,  ( Profile (fromMaybe "" name) Nothing Nothing Nothing, datetime))
tagToProfile _ _ = Nothing

sendPost :: TChan Request -> HomeModel -> (HomeEvent -> IO ()) -> IO ()
sendPost channel model _ = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = model ^. myKeys
  let unsigned = textNote (strip $ model ^. noteInput) xo now;
  atomically $ writeTChan channel $ SendEvent $ signEvent unsigned kp xo

buildUI
  :: HomeWenv
  -> HomeModel
  -> HomeNode
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "New Post"
        , spacer
        , vstack
            [ hstack
                [ textArea noteInput
                  `nodeKey` "noteInput"
                  `styleBasic` [ height 50 ]
                , filler
                , button "Post" SendPost
                    `nodeEnabled` (strip (model ^. noteInput) /= "")
                ]
            ]
        , spacer
        , ViewPosts.viewPostsWidget
            wenv
            viewPostsModel
            (\_ -> True)
            ViewPostDetails
            ViewProfile
        ]

addEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r `sameRelay` r') rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)
