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
  , _time             :: DateTime
  , _events           :: [ReceivedEvent]
  , _contacts         :: Map XOnlyPubKey (Profile, DateTime)
  , _noteInput        :: Text
  , _homeSub          :: SubscriptionId
  , _initSub          :: SubscriptionId
  , _viewPostsModel   :: ViewPosts.ViewPostsModel
  } deriving (Eq, Show)

instance Default HomeModel where
  def = HomeModel initialKeys "" (fromSeconds 0) [] Map.empty "" "" "" def

data HomeEvent
  -- subscriptions
  = LoadContacts -- @todo: add waiting for relay connection loading indicator
  | ContactsLoaded (Map XOnlyPubKey (Profile, DateTime))
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
homeWidget channel pool model = composite_ "HomeWidget" model buildUI (handleHomeEvent channel pool) [ onInit LoadContacts ]

handleHomeEvent
  :: TChan Request
  -> MVar RelayPool
  -> HomeWenv
  -> HomeNode
  -> HomeModel
  -> HomeEvent
  -> [EventResponse HomeModel HomeEvent sp ep]
handleHomeEvent channel pool env node model evt = case evt of
  LoadContacts ->
    [ Producer $ loadContacts channel pool model ]
  ContactsLoaded cs ->
    [ Model $ model & contacts .~ cs
    , Producer $ initSubscriptions channel pool model
    ]
  SendPost ->
    [ Model $ model
        & noteInput .~ ""
    , Producer $ sendPost channel model
    ]
  ViewPostDetails re ->
    []
  ViewProfile xo ->
    []

initSubscriptions :: TChan Request -> MVar RelayPool -> HomeModel -> (HomeEvent -> IO ()) -> IO ()
initSubscriptions request pool model sendMsg = do
  response <- atomically newTChan
  subId <- subscribe pool request response initialFilters
  void . forkIO $ void . forever $ do
    msg <- atomically $ readTChan response
    case msg of
      (EventReceived _ event) -> do
        case kind event of
          Contacts -> do
            let contacts = Map.fromList $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
            return ()
          _ -> putStrLn "Unexpected event kind received when loading contacts" -- @todo handle differently

      _ -> mzero
    where
      (Keys _ xo _ _) = model ^. myKeys
      initialFilters = [ ContactsFilter [ xo ] ] -- @todo update this list

loadContacts :: TChan Request -> MVar RelayPool -> HomeModel -> (HomeEvent -> IO ()) -> IO ()
loadContacts request pool model sendMsg = do
  response <- atomically newTChan
  subId <- subscribe pool request response [ ContactsFilter [ xo ] ]
  msg <- atomically $ readTChan response
  case msg of
    (EventReceived _ event) -> do
      case kind event of
        Contacts -> do
          unsubscribe pool request subId
          let contacts = Map.fromList $ catMaybes $ map (tagToProfile $ created_at event) (tags event)
          sendMsg $ ContactsLoaded contacts
        _ -> putStrLn "Unexpected event kind received when loading contacts" -- @todo handle differently
    _ -> mzero
  where
    (Keys _ xo _ _) = model ^. myKeys

tagToProfile :: DateTime -> Tag -> Maybe (XOnlyPubKey, (Profile, DateTime))
tagToProfile datetime (PTag (ValidXOnlyPubKey xo) _ name) = Just (xo,  ( Profile (fromMaybe "" name) Nothing Nothing Nothing, datetime))
tagToProfile _ _ = Nothing

--addContact :: [Profile] -> Event -> [Profile]
--addContact profiles e = profiles ++ newProfiles
--  where
--    newProfiles = tagsToProfiles (tags e)
--    newProfiles' = filter (\p -> not $ p `elem` profiles)

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
        ]
--        , ViewPosts.viewPostsWidget
--            wenv
--            viewPostsModel
--            (\re -> kind (fst re) == TextNote
--              && pubKey (fst re) `elem` (xo : contacts')
--            )
--            ViewPostDetails
--            ViewProfile
--        ]
--        where
--          (Keys _ xo _ _) = fromJust $ model ^. myKeys
--          contacts' = map extractXOFromProfile (model ^. contacts)

handleReceivedEvent :: HomeModel -> Event -> Relay -> HomeModel
handleReceivedEvent model e r =
  case kind e of
    TextNote ->
      model
        & viewPostsModel . ViewPosts.events .~ newEvents
        & events .~ newEvents
      where
        newEvents = addEvent (model ^. events) e r
--    Metadata ->
--      model
--        & myKeys .~ Just (updateName (fromJust $ model ^. myKeys))
--        & editProfileModel . EditProfile.inputs . EditProfile.nameInput .~ name'
--        & editProfileModel . EditProfile.inputs . EditProfile.aboutInput .~ about'
--        & editProfileModel . EditProfile.inputs . EditProfile.pictureUrlInput .~ pictureUrl'
--      where
--        mp = decode $ LazyBytes.fromStrict $ encodeUtf8 $ content e :: Maybe Profile
--        name' = maybe "" name mp
--        about' = maybe "" about mp
--        pictureUrl' = maybe "" pictureUrl mp
--        xo' = pubKey e
--        updateName (Keys kp xo a n) = if xo == xo'
--          then Keys kp xo a (Just name')
--          else Keys kp xo a n
--    Contacts ->
--      if not $ model ^. isInitialized
--        then
--          model
--            & isInitialized .~ True
--            & initSub .~ ""
--            & contacts .~ newContacts
--            & viewPostsModel . ViewPosts.contacts .~ newContacts
--        else
--          model
--            & contacts .~ newContacts
--            & viewPostsModel . ViewPosts.contacts .~ newContacts
--      where
--        newContacts = addContact (model ^. contacts) e
    Delete -> -- @todo handle delete events
      model
    _ ->
      model

addEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r `sameRelay` r') rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)
