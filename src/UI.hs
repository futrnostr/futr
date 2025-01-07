{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Control.Monad.Fix (mfix)
import Data.Aeson (decode, eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text, drop, pack, unpack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (get, modify)
import Effectful.TH
import QtQuick
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Prelude hiding (drop)
import Text.Read (readMaybe)
import Text.Regex.TDFA

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (createMetadata)
import Nostr.Publisher
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types (Event(..), EventId(..), Kind(..), Profile(..), Relationship(..), Rumor(..), Tag(..))
import Nostr.Util
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Store.Lmdb (TimelineType(..), getEvent, getFollows, getProfile, getTimelineIds)
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

    profileClass <- newClass [
        defPropertySigRO' "name" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ name profile,

        defPropertySigRO' "displayName" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ displayName profile,

        defPropertySigRO' "about" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ about profile,

        defPropertySigRO' "picture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ picture profile,

        defPropertySigRO' "nip05" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ nip05 profile,

        defPropertySigRO' "banner" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ banner profile,

        defPropertySigRO' "isFollow" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let currentPubKey = keyPairToPubKeyXO <$> keyPair st
          let profilePubKey = currentProfile st
          case (currentPubKey, profilePubKey) of
            (Just userPK, Just profilePK) -> do
              follows <- runE $ getFollows userPK
              return $ profilePK `elem` map pubkey follows
            _ -> return False
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
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (displayName profile)),
        followProp "name" $ \_ -> maybe (return "") (\follow -> do
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (name profile)),
        followProp "picture" $ \_ -> maybe (return "") (\follow -> do
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (picture profile))
      ]

    followPool <- newFactoryPool (newObject followClass)

    let getRootReference evt =
          case find (\case ETag _ _ (Just Root) -> True; _ -> False) (tags evt) of
            Just (ETag eid _ _) -> return $ Just eid
            _ -> return Nothing

        getParentReference evt =
          case find (\case ETag _ _ (Just Reply) -> True; _ -> False) (tags evt) of
            Just (ETag eid _ _) -> return $ Just eid
            _ -> return Nothing

    postClass <- mfix $ \postClass' -> newClass [
        defPropertySigRO' "id" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          let value = pack $ show eid
          return value,

        defPropertySigRO' "postType" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          eventMaybe <- runE $ getEvent eid
          let value = case eventMaybe of
                Just eventWithRelays ->
                    pack $ case kind (event eventWithRelays) of
                        ShortTextNote ->
                            if any (\t -> case t of
                                          QTag _ _ _ -> True
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
                Repost -> do
                  case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 $ content ev) of
                    Right repostedEvent -> return $ Just $ content repostedEvent
                    Left _ -> return $ Just $ content ev
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

        defPropertySigRO' "author" changeKey' $ \_ -> do
          Just <$> newObject profileClass (),

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
                  eTagRefs = [tagId | ETag tagId _ _ <- tags ev]
                  qTagRefs = [tagId | QTag tagId _ _ <- tags ev]
                  contentRefs = extractNostrReferences (content ev)
                  allRefs = nub $ eTagRefs ++ qTagRefs ++ contentRefs
              mapM (newObject postClass') allRefs
            Nothing -> return []
      ]

    postsPool <- newFactoryPool (newObject postClass)
    chatPool <- newFactoryPool (newObject postClass)

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertyConst' "ctxRelayMgmt" (\_ -> return relayMgmtObj),

        defPropertyConst' "currentProfile" (\_ -> do
          profileObj <- newObject profileClass ()
          runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { profileObjRef = Just profileObj } }
          return profileObj
        ),

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
              (profile', _) <- runE $ getProfile $ keyPairToPubKeyXO kp
              return $ fromMaybe "" $ picture profile'
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
              runE $ logInfo "Profile successfully saved and sent to relay pool"
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

        defMethod' "follow" $ \_ npubText -> runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> runE $ unfollowProfile npubText,

        defMethod' "openChat" $ \_ npubText -> runE $ do
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npubText
            openChat pubKeyXO,

        defMethod' "sendPrivateMessage" $ \_ input -> runE $ sendPrivateMessage input, -- NIP-17 private direct message

        defMethod' "sendShortTextNote" $ \_ input -> runE $ sendShortTextNote input, -- NIP-01 short text note

        defMethod' "repost" $ \_ eid -> runE $ do -- NIP-18 repost
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          repost eid',

        defMethod' "quoteRepost" $ \_ eid quote -> runE $ do -- NIP-18 quote repost
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          quoteRepost eid' quote,

        defMethod' "comment" $ \_ eid input -> runE $ do -- NIP-22 comment
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          comment eid' input,

        defMethod' "deleteEvent" $ \_ eid input -> runE $ do -- NIP-09 delete post
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          deleteEvent eid' input,

        defMethod' "getPost" $ \_ eid -> do
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          postObj <- newObject postClass eid'
          return postObj
      ]

    rootObj <- newObject rootClass ()

    return rootObj

-- Helper function to extract nostr: references from content
extractNostrReferences :: Text -> [EventId]
extractNostrReferences txt =
    let matches = txt =~ ("nostr:(note|nevent)1[a-zA-Z0-9]+" :: Text) :: [[Text]]
        refs = mapMaybe (bech32ToEventId . drop 6 . head) matches  -- drop "nostr:" prefix
    in refs
