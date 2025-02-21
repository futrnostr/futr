{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Control.Monad (filterM)
import Control.Monad.Fix (mfix)
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Set qualified as Set
import Data.Text (Text, drop, pack, unpack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent.STM (TQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (get, gets, modify)
import Effectful.TH
import QtQuick
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Prelude hiding (drop)
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event ( Event(..), EventId(..), Kind(..), Rumor(..)
                   , createMetadata, eventIdFromHex )
import Nostr.Publisher
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.Relay (RelayURI)
import Nostr.Util
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Store.Lmdb (TimelineType(..), getCommentIds, getEvent, getFollows, getProfile, getTimelineIds)
import TimeFormatter
import Types


-- | Key Management Effect for creating QML UI.
data UI :: Effect where
  CreateUI :: SignalKey (IO ()) -> UI m (ObjRef ())


-- | Dispatch type for UI effect.
type instance DispatchOf UI = Dynamic


makeEffect ''UI


-- | Run the UI effect.
runUI :: (FutrEff es, Futr :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ KeyMgmtUI.createUI changeKey'
    relayMgmtObj <- runE $ RelayMgmtUI.createUI changeKey'

    let getProfileEventCount :: (PubKeyXO -> RelayURI -> Eff es (Maybe (TQueue SubscriptionEvent))) 
                            -> PubKeyXO 
                            -> Eff es Int
        getProfileEventCount subscriber pubkey = do
            return 0 -- @todo implement

    profileClass <- newClass [
        defPropertySigRO' "id" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          let value = TE.decodeUtf8 $ B16.encode $ exportPubKeyXO pk
          return value,

        defPropertySigRO' "npub" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          return $ pubKeyXOToBech32 pk,

        defPropertySigRO' "name" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ name profile,

        defPropertySigRO' "displayName" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ displayName profile,

        defPropertySigRO' "about" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ about profile,

        defPropertySigRO' "picture" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ picture profile,

        defPropertySigRO' "nip05" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ nip05 profile,

        defPropertySigRO' "banner" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          profile <- runE $ getProfile pk
          return $ banner profile,

        defPropertySigRO' "isFollow" changeKey' $ \obj -> do
          let pk = fromObjRef obj :: PubKeyXO
          kp <- runE getKeyPair
          let currentPubKey = keyPairToPubKeyXO kp
          follows <- runE $ getFollows currentPubKey
          return $ pk `elem` map pubkey follows,

        defPropertySigRO' "followerCount" changeKey' $ \obj -> do
            return (0 :: Int),
            {-
            st <- runE $ get @AppState
            let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
            runE $ getProfileEventCount subscribeToFollowers pk,
            -}

        defPropertySigRO' "followingCount" changeKey' $ \obj -> do
            return (0 :: Int)
            {-
            st <- runE $ get @AppState
            let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
            runE $ getProfileEventCount subscribeToFollowing pk
            -}
      ]

    let followProp name' accessor = defPropertySigRO' name' changeKey' $ \obj -> do
          let pubKeyXO = fromObjRef obj :: PubKeyXO
          st <- runE $ get @AppState
          followData <- case keyPairToPubKeyXO <$> keyPair st of
            Just upk | upk == pubKeyXO ->
              return $ Just Follow { pubkey = pubKeyXO, petName = Nothing }
            Just upk -> do
              follows <- runE $ getFollows upk
              if pubKeyXO `elem` map pubkey follows
                then return $ Just Follow { pubkey = pubKeyXO, petName = Nothing }
                else return Nothing
            Nothing -> return Nothing
          accessor st followData

    followClass <- newClass [
        followProp "pubkey" $ \_ -> return . maybe "" (pubKeyXOToBech32 . pubkey),
        followProp "petname" $ \_ -> return . maybe "" (fromMaybe "" . petName),
        followProp "displayName" $ \_ -> maybe (return "") (\follow -> do
            profile <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (displayName profile)),
        followProp "name" $ \_ -> maybe (return "") (\follow -> do
            profile <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (name profile)),
        followProp "picture" $ \_ -> maybe (return "") (\follow -> do
            profile <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (picture profile))
      ]

    followPool <- newFactoryPool (newObject followClass)

    let getRootReference evt =
          case find (\case ("e":_:"root":_) -> True; _ -> False) (tags evt) of
            Just ("e":eidHex:_) -> return $ eventIdFromHex eidHex
            _ -> return Nothing

        getParentReference evt =
          case find (\case ("e":_:"reply":_) -> True; _ -> False) (tags evt) of
            Just ("e":eidHex:_) -> return $ eventIdFromHex eidHex
            _ -> return Nothing
{-
        getEventCount subscriber postId = do
            eventMaybe <- getEvent postId
            case eventMaybe of
                Just EventWithRelays{relays} -> do
                    case Set.toList relays of
                        [] -> return 0
                        (relay:_) -> do
                            mQueue <- subscriber postId relay
                            case mQueue of
                                Just queue -> countEvents queue
                                Nothing -> return 0
                Nothing -> return 0
-}
    postClass <- mfix $ \postClass' -> newClass [
        defPropertyRO' "id" $ \obj -> do
          let eid = fromObjRef obj :: EventId
          let value = TE.decodeUtf8 $ B16.encode $ getEventId eid
          return value,

        defPropertyRO' "nevent" $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          case eventMaybe of
            Just eventWithRelays -> do
              let e = event eventWithRelays
              let r = head $ Set.toList $ relays eventWithRelays
              runE $ logDebug $ "nevent: " <> pack (show e)
              return $ eventToNevent e (Just r)
            Nothing -> return "",

        defPropertySigRO' "raw" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          case eventMaybe of
            Just eventWithRelays -> do
              let e = event eventWithRelays
                  prettyConfig = defConfig {
                    confCompare = keyOrder [ "id", "pubkey", "created_at", "kind", "tags", "content", "sig"]
                      `mappend` compare
                  }
                  prettyEncode x = TE.decodeUtf8 . BSL.toStrict $ encodePretty' prettyConfig (toJSON x)
              case kind e of
                GiftWrap -> do
                  kp <- runE getKeyPair
                  sealed <- runE $ unwrapGiftWrap e kp
                  rumor <- runE $ maybe (return Nothing) (\s -> unwrapSeal s kp) sealed
                  return $ [prettyEncode e]
                       ++ maybe [] ((:[]) . prettyEncode) sealed
                       ++ maybe [] ((:[]) . prettyEncode) rumor
                _ -> return [prettyEncode e]
            Nothing -> return [],

        defPropertySigRO' "relays" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          case eventMaybe of
            Just eventWithRelays -> do
              return $ Set.toList $ relays eventWithRelays
            Nothing -> return [],

        defPropertySigRO' "postType" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          let value = case eventMaybe of
                Just eventWithRelays ->
                    pack $ case kind (event eventWithRelays) of
                        ShortTextNote ->
                            if any (\t -> case t of
                                          ("q":_) -> True
                                          _ -> False) (tags (event eventWithRelays))
                            then "quote_repost"
                            else "short_text_note"
                        Repost -> "repost"
                        Comment -> "comment"
                        GiftWrap -> "gift_wrap"
                        DirectMessage -> "direct_message"
                        _ -> "unknown"
                Nothing -> "unknown"
          return value,

        defPropertySigRO' "content" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          value <- runE $ getEvent eid >>= \case
            Just eventWithRelays -> do
              let ev = event eventWithRelays
              case kind ev of
                GiftWrap -> do
                  kp <- getKeyPair
                  sealed <- unwrapGiftWrap ev kp
                  rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
                  return $ rumorContent <$> rumor
                _ -> return $ Just $ content ev
            Nothing -> return Nothing
          return value,

        defPropertySigRO' "timestamp" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          value <- case eventMaybe of
            Just eventWithRelays -> do
              now <- runE getCurrentTime
              return $ Just $ formatDateTime English now (createdAt (event eventWithRelays))
            Nothing -> return Nothing
          return value,

        defPropertySigRO' "author" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          value <- runE $ getEvent eid >>= \case
            Just eventWithRelays -> do
              let ev = event eventWithRelays
              case kind ev of
                GiftWrap -> do
                  kp <- getKeyPair
                  sealed <- unwrapGiftWrap ev kp
                  rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
                  return $ rumorPubKey <$> rumor
                _ -> return $ Just $ pubKey ev
            Nothing -> return Nothing
          case value of
            Just pk -> Just <$> newObject profileClass pk
            Nothing -> return Nothing,

        -- For comments: points to the original post that started the thread
        -- Example: Post A <- Comment B <- Comment C
        --          Comment C's rootPost is Post A
        defPropertySigRO' "root" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          case eventMaybe of
            Just eventWithRelays -> getRootReference (event eventWithRelays) >>= \case
              Just refId -> Just <$> newObject postClass' refId
              Nothing -> return Nothing
            Nothing -> return Nothing,

        -- For nested comments: points to the immediate parent comment when different from root
        -- Example: Post A <- Comment B <- Comment C
        --          Comment C's parentPost is Comment B
        --          Comment B's parentPost is null (same as root)
        defPropertySigRO' "parent" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          case eventMaybe of
            Just eventWithRelays -> do
              parentId <- runE $ getParentReference (event eventWithRelays)
              rootId <- runE $ getRootReference (event eventWithRelays)
              case (parentId, rootId) of
                (Just p, Just r) | p /= r -> Just <$> newObject postClass' p
                _ -> return Nothing
            Nothing -> return Nothing,

        -- Referenced posts property
        defPropertySigRO' "referencedPosts" changeKey' $ \obj -> do
          let postId = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent postId
          case eventMaybe of
            Just eventWithRelays -> do
              let ev = event eventWithRelays
                  eTagRefs = [ eid | ("e":eidHex:_) <- tags ev
                                 , Just eid <- [eventIdFromHex eidHex] ]
                  qTagRefs = [ eid | ("q":eidHex:_) <- tags ev
                                 , Just eid <- [eventIdFromHex eidHex] ]
                  contentRefs = extractNostrReferences (content ev)
                  allRefs = nub $ eTagRefs ++ qTagRefs ++ contentRefs
              mapM (newObject postClass') allRefs
            Nothing -> return [],

        -- Event count properties using the helper
        defPropertySigRO' "repostCount" changeKey' $ \_ -> do
            return (0 :: Int),
            --runE $ getEventCount subscribeToReposts (fromObjRef obj),

        defPropertySigRO' "comments" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { currentPostCommentsObjRef = Just obj } }
          let postId = fromObjRef obj :: EventId
          commentIds <- runE $ getCommentIds postId
          mapM (newObject postClass') commentIds
      ]

    postsPool <- newFactoryPool (newObject postClass)
    chatPool <- newFactoryPool (newObject postClass)

    publishStatusClass <- newClass [
        defPropertySigRO' "eventId" changeKey'$ \obj -> do
            let eid = fromObjRef obj :: EventId
            return $ TE.decodeUtf8 $ B16.encode $ getEventId eid,

        defPropertySigRO' "relayStatuses" changeKey' $ \obj -> do
            let eid = fromObjRef obj :: EventId
            st <- runE $ get @RelayPool
            return $ case Map.lookup eid (publishStatus st) of
                Just statusMap ->
                    [ [relay, if status == Success then ("" :: Text) else
                        case status of
                            Failure msg -> msg
                            _ -> ""]
                    | (relay, status) <- Map.toList statusMap ]
                Nothing -> []
        ]

    -- Create a pool for publish statuses
    publishStatusPool <- newFactoryPool (newObject publishStatusClass)

    rootClass <- newClass [
        defPropertyConst' "version" (\_ -> do
          st <- runE $ get @AppState
          return $ version st
        ),

        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertyConst' "ctxRelayMgmt" (\_ -> return relayMgmtObj),

        defPropertyConst' "currentProfile" (\_ -> do
          mp <- runE $ gets @AppState currentProfile
          case mp of
            Just pk -> do
              profileObj <- newObject profileClass pk
              runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { profileObjRef = Just profileObj } }
              return $ Just profileObj
            Nothing -> return Nothing),

        defPropertySigRW' "currentScreen" changeKey'
            (\_ -> do
              st <- runE $ get @AppState
              return $ pack $ show $ currentScreen st)
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        runE $ do
                          modify $ \st -> st { currentScreen = s }
                          fireSignal obj
                    Nothing -> return ()),

        defPropertySigRO' "mynpub" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          return $ case keyPair st of
            Just kp -> pubKeyXOToBech32 $ keyPairToPubKeyXO kp
            Nothing -> "",

        defPropertySigRO' "mypicture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          case keyPair st of
            Just kp -> do
              profile <- runE $ getProfile $ keyPairToPubKeyXO kp
              return $ fromMaybe "" $ picture profile
            Nothing -> return "",

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "logout" $ \obj -> runE $ logout obj,

        defMethod' "search" $ \obj input -> runE $ do
          res <- search obj input
          return $ TE.decodeUtf8 $ BSL.toStrict $ encode res,

        defMethod' "setCurrentProfile" $ \_ npub' -> runE $ setCurrentProfile npub',

        defMethod' "saveProfile" $ \_ input -> do
          let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
          n <- runE getCurrentTime
          kp <- runE getKeyPair
          let unsigned = createMetadata profile (keyPairToPubKeyXO kp) n
          signedMaybe <- runE $ signEvent unsigned kp
          case signedMaybe of
            Just signed -> do
              runE $ broadcast signed
            Nothing -> runE $ logWarning "Failed to sign profile update event",

        defPropertySigRO' "followList" changeKey' $ \obj -> do
          runE $ modify $ \s -> s { uiRefs = (uiRefs s) { followsObjRef = Just obj } }
          st <- runE $ get @AppState
          case keyPair st of
            Just kp -> do
              let userPubKey = keyPairToPubKeyXO kp
              followedPubKeys <- runE $ getFollows userPubKey
              mapM (getPoolObject followPool) (userPubKey : map pubkey followedPubKeys)
            _ -> return [],

        defPropertySigRO' "posts" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { postsObjRef = Just obj } }
          st <- runE $ get @AppState
          case fst (currentContact st) of
            Just recipient -> do
                postIds <- runE $ getTimelineIds PostTimeline recipient 1000
                mapM (getPoolObject postsPool) postIds
            Nothing -> return [],

        defPropertySigRO' "privateMessages" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { privateMessagesObjRef = Just obj } }
          st <- runE $ get @AppState
          case fst (currentContact st) of
            Just recipient -> do
                messageIds <- runE $ getTimelineIds ChatTimeline recipient 1000
                mapM (getPoolObject chatPool) messageIds
            Nothing -> return [],

        defPropertySigRO' "publishStatuses" changeKey' $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { publishStatusObjRef = Just obj } }
            st <- runE $ get @RelayPool
            now <- runE getCurrentTime
            -- Filter statuses where event is newer than 10 seconds
            let statusMap = publishStatus st
            recentEids <- filterM (\eid -> do
                eventMaybe <- runE $ getEvent eid
                case eventMaybe of
                    Just eventWithRelays ->
                        return $ createdAt (event eventWithRelays) + 10 > now
                    Nothing -> return False) (Map.keys statusMap)
            mapM (getPoolObject publishStatusPool) recentEids,

        defMethod' "follow" $ \_ npubText -> runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> runE $ unfollowProfile npubText,

        defMethod' "openChat" $ \_ npubText -> runE $ do
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npubText
            openChat pubKeyXO,

        defMethod' "sendPrivateMessage" $ \_ input -> runE $ sendPrivateMessage input, -- NIP-17 private direct message

        defMethod' "sendShortTextNote" $ \_ input -> runE $ sendShortTextNote input, -- NIP-01 short text note

        defMethod' "repost" $ \_ eid -> runE $ do -- NIP-18 repost
          let eid' = read (unpack eid) :: EventId
          repost eid',

        defMethod' "quoteRepost" $ \_ eid quote -> runE $ do -- NIP-18 quote repost
          let eid' = read (unpack eid) :: EventId
          quoteRepost eid' quote,

        defMethod' "comment" $ \_ eid input -> runE $ do -- NIP-22 comment
          let eid' = read (unpack eid) :: EventId
          comment eid' input,

        defMethod' "deleteEvent" $ \_ eid input -> runE $ do -- NIP-09 delete post
          let eid' = read (unpack eid) :: EventId
          deleteEvent eid' input,

        defMethod' "setCurrentPost" $ \_ meid -> runE $ do
          case meid of
            Just eid -> do
              let eid' = read (unpack eid) :: EventId
              setCurrentPost $ Just eid'
            Nothing -> setCurrentPost Nothing
      ]

    rootObj <- newObject rootClass ()

    return rootObj

-- Helper function to extract nostr: references from content
extractNostrReferences :: Text -> [EventId]
extractNostrReferences txt =
    let matches = txt =~ ("nostr:(note|nevent)1[a-zA-Z0-9]+" :: Text) :: [[Text]]
        refs = mapMaybe (bech32ToEventId . drop 6 . head) matches  -- drop "nostr:" prefix
    in refs
