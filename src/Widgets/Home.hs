{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.Home where

import Control.Concurrent           (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad                (forever, void)
import Control.Monad.STM            (atomically)
import Crypto.Schnorr               (XOnlyPubKey)
import Data.Aeson
import Data.DateTime
import Data.Default
import Data.List                    (find, sortBy)
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
import Nostr.Request

import qualified Widgets.ViewPosts    as ViewPosts

type HomeWenv = WidgetEnv HomeModel HomeEvent

type HomeNode = WidgetNode HomeModel HomeEvent

data HomeModel = HomeModel
  { _keys            :: Maybe Keys
  , _time            :: DateTime
  , _events          :: [ReceivedEvent]
  , _contacts        :: [Profile]
  , _noteInput       :: Text
  , _isInitialized   :: Bool
  , _homeSub         :: SubscriptionId
  , _initSub         :: SubscriptionId
  , _viewPostsModel  :: ViewPosts.ViewPostsModel
  } deriving (Eq, Show)

instance Default HomeModel where
  def = HomeModel Nothing (fromSeconds 0) [] [] "" False "" "" def

data HomeEvent
  = NoOp
  | TimerTick DateTime
  | Initialize
  | InitSubscribed SubscriptionId
  | HomeFilterSubscribed SubscriptionId
  | EventAppeared Event Relay
  | SendPost
  | ViewPostDetails ReceivedEvent
  | ViewProfile XOnlyPubKey
  deriving (Eq, Show)

makeLenses 'HomeModel

homeWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan Request
  -> ALens' sp HomeModel
  -> WidgetNode sp ep
homeWidget chan model = composite "homeWidget" model viewHome (handleHomeEvent chan)

handleHomeEvent
  :: TChan Request
  -> HomeWenv
  -> HomeNode
  -> HomeModel
  -> HomeEvent
  -> [EventResponse HomeModel HomeEvent sp ep]
handleHomeEvent chan env node model evt = case evt of
  NoOp ->
    []
  TimerTick now ->
    [ Model $ model & time .~ now ]
  Initialize ->
    [ Task $ sendInitFilter chan (fromJust $ model ^.keys)
    , Producer timerLoop
    ]
  InitSubscribed subId ->
    [ Model $ model & initSub .~ subId ]
  EventAppeared e r -> do
    let newModel = handleEvent model e r
    if not $ model ^. isInitialized && model ^. isInitialized
      then
        [ Model $ newModel
        , Task $ sendHomeFilters chan model
        ]
      else
        [ Model $ newModel ]
  HomeFilterSubscribed subId ->
    [ Model $ model & homeSub .~ subId ]
  SendPost ->
    [ Model $ model
        & noteInput .~ ""
    , Task $ sendPost chan model
    ]
  ViewPostDetails re ->
    []
  ViewProfile xo ->
    []

sendInitFilter :: TChan Request -> Keys -> IO HomeEvent
sendInitFilter channel (Keys _ xo _ _) = do
  subId <- subscribe channel [InitialFilter xo]
  return $ InitSubscribed subId

sendHomeFilters :: TChan Request -> HomeModel -> IO HomeEvent
sendHomeFilters channel model = do
  subId <- subscribe channel [TextNoteFilter xoList, ContactsFilter xoList]
  return $ HomeFilterSubscribed subId
  where
    (Keys _ xo _ _) = fromJust $ model ^. keys
    contactList = map extractXOFromProfile (model ^. contacts)
    xoList = xo : contactList

handleEvent :: HomeModel -> Event -> Relay -> HomeModel
handleEvent model e r =
  case kind e of
    TextNote ->
      model
        & viewPostsModel . ViewPosts.events .~ newEvents
        & events .~ newEvents
      where
        newEvents = addEvent (model ^. events) e r
    Metadata ->
      model
        & keys .~ Just (updateName (fromJust $ model ^. keys))
      where
        mp = decode $ LazyBytes.fromStrict $ encodeUtf8 $ content e :: Maybe ProfileData
        name' = maybe "" name mp
        xo' = pubKey e
        updateName (Keys kp xo a n) = if xo == xo'
          then Keys kp xo a (Just name')
          else Keys kp xo a n
    Contacts ->
      if not $ model ^. isInitialized
        then
          model
            & isInitialized .~ True
            & initSub .~ ""
            & contacts .~ newContacts
            & viewPostsModel . ViewPosts.contacts .~ newContacts
        else
          model
            & contacts .~ newContacts
            & viewPostsModel . ViewPosts.contacts .~ newContacts
      where
        newContacts = addContact (model ^. contacts) e
    Delete -> -- @todo handle delete events
      model
    _ ->
      model

addEvent :: [ReceivedEvent] -> Event -> Relay -> [ReceivedEvent]
addEvent re e r = sortBy sortByDate $ addedEvent : newList
  where
    addedEvent = case find (dupEvent e) re of
      Just (e', rs) -> (e', r : filter (\r' -> not $ r' == r) rs)
      _             -> (e, [r])
    newList = filter (not . dupEvent e) re
    dupEvent e' re' = e' == fst re'
    sortByDate a b = compare (created_at $ fst b) (created_at $ fst a)

addContact :: [Profile] -> Event -> [Profile]
addContact profiles e = profiles ++ newProfiles
  where
    newProfiles = tagsToProfiles (tags e)
    newProfiles' = filter (\p -> not $ p `elem` profiles)

sendPost :: TChan Request -> HomeModel -> IO HomeEvent
sendPost chan model = do
  now <- getCurrentTime
  let (Keys kp xo _ _) = fromJust $ model ^. keys
  let unsigned = textNote (strip $ model ^. noteInput) xo now;
  atomically $ writeTChan chan $ SendEvent $ signEvent unsigned kp xo
  return NoOp

timerLoop :: (HomeEvent -> IO ()) -> IO ()
timerLoop sendMsg = void . forkIO $ void $ forever $ do
  now <- getCurrentTime
  sendMsg $ TimerTick now
  threadDelay 1000000

viewHome
  :: HomeWenv
  -> HomeModel
  -> HomeNode
viewHome wenv model = widgetTree
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
            (\re -> kind (fst re) == TextNote
              && pubKey (fst re) `elem` (xo : contacts')
            )
            ViewPostDetails
            ViewProfile
        ]
        where
          (Keys _ xo _ _) = fromJust $ model ^. keys
          contacts' = map extractXOFromProfile (model ^. contacts)
