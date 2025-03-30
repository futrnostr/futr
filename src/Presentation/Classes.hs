module Presentation.Classes where

import Control.Monad.Fix (mfix)
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.List (find, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Graphics.QML
import System.FilePath ((</>), takeExtension, dropExtension)

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), Kind(..), Rumor(..), eventIdFromHex)
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.Util (Util, getCurrentTime, getKeyPair)
import QtQuick (QtQuick, PropertyMap(..), QtQuickState(..), UIReferences(..))
import Store.Lmdb (LmdbStore, getCommentIds, getEvent, getFollows, getProfile)
import TimeFormatter
import Types (AppState(..), EventWithRelays(..), Follow(..), PublishStatus(..), RelayPool(..))

data Classes :: Effect where
    ProfileClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    PostClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    PublishStatusClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    FollowClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)


type instance DispatchOf Classes = Dynamic

makeEffect ''Classes

-- | SubscriptionEff
type ClassesEff es =
  ( QtQuick :> es
  , State QtQuickState :> es
  , State RelayPool :> es
  , State AppState :> es
  , LmdbStore :> es
  , Concurrent :> es
  , Nostr :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Handler for subscription effects.
runClasses
  :: ClassesEff es
  => Eff (Classes : es) a
  -> Eff es a
runClasses = interpret $ \_ -> \case
    ProfileClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "id" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                let value = TE.decodeUtf8 $ B16.encode $ exportPubKeyXO pk
                return value,

            defPropertySigRO' "npub" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                return $ pubKeyXOToBech32 pk,

            defPropertySigRO' "name" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ name profile,

            defPropertySigRO' "displayName" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ displayName profile,

            defPropertySigRO' "about" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ about profile,

            defPropertySigRO' "picture" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ picture profile,

            defPropertySigRO' "nip05" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ nip05 profile,

            defPropertySigRO' "banner" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                profile <- runE $ getProfile pk
                return $ banner profile,

            defPropertySigRO' "isFollow" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk obj
                kp <- runE getKeyPair
                let currentPubKey = keyPairToPubKeyXO kp
                follows <- runE $ getFollows currentPubKey
                return $ pk `elem` map pubkey follows,

            defPropertySigRO' "followerCount" changeKey' $ \_ -> do
                return (0 :: Int),
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowers pk,
                -}

            defPropertySigRO' "followingCount" changeKey' $ \_ -> do
                return (0 :: Int)
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowing pk
                -}
            ]


    PostClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        let getRootReference evt =
                case find (\case ("e":_:"root":_) -> True; _ -> False) (tags evt) of
                    Just ("e":eidHex:_) -> return $ eventIdFromHex eidHex
                    _ -> return Nothing

            getParentReference evt =
                case find (\case ("e":_:"reply":_) -> True; _ -> False) (tags evt) of
                    Just ("e":eidHex:_) -> return $ eventIdFromHex eidHex
                    _ -> return Nothing


        mfix $ \postClass' -> newClass [
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
                  return $ eventToNevent e [r]
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
              runE $ storePostObjRef eid obj
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
                    Repost -> return $ Just ""
                    _ -> return $ Just $ content ev
                Nothing -> return Nothing
              return value,

            defPropertySigRO' "contentParts" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              value <- runE $ getEvent eid >>= \case
                Just eventWithRelays -> do
                  let ev = event eventWithRelays
                  content' <- case kind ev of
                    GiftWrap -> do
                      kp <- getKeyPair
                      sealed <- unwrapGiftWrap ev kp
                      rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
                      return $ fromMaybe "" $ rumorContent <$> rumor
                    Repost -> do
                      let repostedId = listToMaybe [ eidStr | ("e":eidStr:_) <- tags ev ]
                      case repostedId of
                        Just eid' -> do
                          let eid'' = read (unpack eid') :: EventId
                          return $ "nostr:" <> (eventIdToNote eid'')
                        Nothing -> return ""
                    _ -> return $ content ev
                  r <- parseContentParts content'
                  return r
                Nothing -> return []
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

            defPropertySigRO' "authorId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              runE $ getEvent eid >>= \case
                Just eventWithRelays -> do
                  let ev = event eventWithRelays
                  case kind ev of
                    GiftWrap -> do
                      kp <- getKeyPair
                      sealed <- unwrapGiftWrap ev kp
                      rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
                      return $ pubKeyXOToBech32 <$> (rumorPubKey <$> rumor)
                    _ -> return $ Just $ pubKeyXOToBech32 $ pubKey ev
                Nothing -> return Nothing,

            defPropertySigRO' "root" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just eventWithRelays -> runE $ getRootReference (event eventWithRelays) >>= \case
                  Just refId -> return $ Just $ eventIdToNote refId
                  Nothing -> return Nothing
                Nothing -> return Nothing,

            defPropertySigRO' "parent" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just eventWithRelays -> do
                  parentId <- runE $ getParentReference (event eventWithRelays)
                  rootId <- runE $ getRootReference (event eventWithRelays)
                  case (parentId, rootId) of
                    (Just p, Just r) | p /= r -> return $ Just $ eventIdToNote p
                    _ -> return Nothing
                Nothing -> return Nothing,

            defPropertySigRO' "referencedPostId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just eventWithRelays | kind (event eventWithRelays) == Repost -> do
                  let tags' = tags (event eventWithRelays)
                      repostedId = listToMaybe [eidStr | ("e":eidStr:_) <- tags']
                  return repostedId
                _ -> return Nothing,

            defPropertySigRO' "repostCount" changeKey' $ \_ -> do
                return (0 :: Int),

            defPropertySigRO' "comments" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              runE $ storePostObjRef eid obj
              runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { currentPostCommentsObjRef = Just obj } }
              commentIds <- runE $ getCommentIds eid
              mapM (newObject postClass') commentIds
          ]

    PublishStatusClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "eventId" changeKey' $ \obj -> do
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

    FollowClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
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

        newClass [
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

-- | Store a profile object reference in the property map
storeProfileObjRef :: ClassesEff es => PubKeyXO -> ObjRef PubKeyXO -> Eff es ()
storeProfileObjRef pk obj = withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    weakObjRef <- toWeakObjRef obj

    runE $ modify @QtQuickState $ \st ->
        let currentMap = profileObjRefs (propertyMap st)
            updatedMap = Map.insertWith (++) pk [weakObjRef] currentMap
        in st { propertyMap = (propertyMap st) { profileObjRefs = updatedMap } }

    finalizer <- newObjFinaliser $ \_ -> do
        runE $ modify @QtQuickState $ \st' ->
            let currentMap = profileObjRefs (propertyMap st')
                updatedMap = Map.delete pk currentMap
            in st' { propertyMap = (propertyMap st') { profileObjRefs = updatedMap } }


    addObjFinaliser finalizer obj

-- | Store a post object reference in the property map
storePostObjRef :: ClassesEff es => EventId -> ObjRef EventId -> Eff es ()
storePostObjRef evId obj = withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    weakObjRef <- toWeakObjRef obj

    runE $ modify @QtQuickState $ \st ->
        let currentMap = postObjRefs (propertyMap st)
            updatedMap = Map.insertWith (++) evId [weakObjRef] currentMap
        in st { propertyMap = (propertyMap st) { postObjRefs = updatedMap } }

    finalizer <- newObjFinaliser $ \_ -> do
        runE $ modify @QtQuickState $ \st' ->
            let currentMap = postObjRefs (propertyMap st')
                updatedMap = Map.delete evId currentMap
            in st' { propertyMap = (propertyMap st') { postObjRefs = updatedMap } }


    addObjFinaliser finalizer obj


-- | Parse content into parts (text, images, URLs, and nostr references)
parseContentParts :: (LmdbStore :> es) => Text -> Eff es [[Text]]
parseContentParts contentText
    | Text.null contentText = pure []
    | otherwise = do
        let matches = findMatches contentText
            sortedMatches = sortBy (\(a,_,_) (b,_,_) -> compare a b) matches
            mergedMatches = mergeOverlappingMatches sortedMatches

        rawParts <- processContentWithMatchesM contentText 0 mergedMatches
        pure $ mergeAdjacentTextParts rawParts
  where
    -- Find matches for URLs and nostr references
    findMatches :: Text -> [(Int, Int, Text)]
    findMatches text =
        let httpMatches = findPrefixMatches text "http://" "url"
            httpsMatches = findPrefixMatches text "https://" "url"
            nostrMatches = findNostrMatches text
        in httpMatches ++ httpsMatches ++ nostrMatches

    -- Find all occurrences of a prefix and extract the full entity
    findPrefixMatches :: Text -> Text -> Text -> [(Int, Int, Text)]
    findPrefixMatches text prefix typ =
        let positions = Text.breakOnAll prefix text
        in concatMap (\(before, _) ->
                let startPos = Text.length before
                    fullEntity = extractEntity (Text.drop (Text.length before) text)
                    endPos = startPos + Text.length fullEntity
                    matchType = if typ == "url" then
                                  if shouldRenderAsMedia fullEntity then
                                    if isImageUrl fullEntity
                                    then "image"
                                    else if isVideoUrl fullEntity
                                         then "video"
                                         else typ
                                  else "embed-url"
                                else typ
                in [(startPos, endPos, matchType)]
            ) positions

    -- Find nostr references and categorize them by type
    findNostrMatches :: Text -> [(Int, Int, Text)]
    findNostrMatches text =
        let positions = Text.breakOnAll "nostr:" text
        in concatMap (\(before, _) ->
                let startPos = Text.length before
                    fullEntity = extractEntity (Text.drop (Text.length before) text)
                    endPos = startPos + Text.length fullEntity
                    -- Determine the nostr reference type
                    nostrType = if Text.length fullEntity >= 11
                                then
                                    let prefix = Text.take 5 (Text.drop 6 fullEntity) -- after "nostr:"
                                    in case prefix of
                                        "note1" -> "note"
                                        "npub1" -> "embed-npub"
                                        "nprof" -> "embed-nprofile"
                                        "neven" -> "nevent"
                                        "naddr" -> "naddr"
                                        _ -> "nostr"
                                else "nostr"
                in [(startPos, endPos, nostrType)]
            ) positions

    -- Extract a complete entity from text
    extractEntity :: Text -> Text
    extractEntity txt =
        let validChar c = not (c `elem` (" \t\n\r<>\"'()[]{},;" :: String))
        in Text.takeWhile validChar txt

    isImageUrl :: Text -> Bool
    isImageUrl url = any (`Text.isSuffixOf` url) [".jpg", ".jpeg", ".png", ".gif", ".webp"]

    isVideoUrl :: Text -> Bool
    isVideoUrl url = any (`Text.isSuffixOf` url) [".mp4", ".webm", ".mov", ".avi", ".mkv", ".m4v"]

    shouldRenderAsMedia :: Text -> Bool
    shouldRenderAsMedia url = isImageUrl url || isVideoUrl url

    -- Merge overlapping matches
    mergeOverlappingMatches :: [(Int, Int, Text)] -> [(Int, Int, Text)]
    mergeOverlappingMatches [] = []
    mergeOverlappingMatches [x] = [x]
    mergeOverlappingMatches (x@(start1, end1, typ1):y@(start2, end2, _):rest)
        | start2 < end1 = mergeOverlappingMatches ((start1, max end1 end2, typ1) : rest)
        | otherwise = x : mergeOverlappingMatches (y:rest)

    -- Effectful version of processContentWithMatches that can use GetProfile
    processContentWithMatchesM :: (LmdbStore :> es) => Text -> Int -> [(Int, Int, Text)] -> Eff es [[Text]]
    processContentWithMatchesM text currentPos [] = do
        let remaining = Text.drop currentPos text
        pure $ if Text.null remaining
               then []
               else [["text", replaceNewlines remaining]]

    processContentWithMatchesM text currentPos ((start, end, typ):rest) = do
        let beforeText = Text.take (start - currentPos) (Text.drop currentPos text)
            matchText = Text.take (end - start) (Text.drop start text)
            -- Remove "nostr:" prefix for nostr types
            processedMatchText = if "nostr:" `Text.isPrefixOf` matchText &&
                                   typ `elem` ["note", "embed-npub", "embed-nprofile", "nevent", "naddr"]
                                then Text.drop 6 matchText
                                else matchText

        beforePart <- if Text.null beforeText
                      then pure []
                      else pure [["text", replaceNewlines beforeText]]

        -- Handle match part based on type
        matchPart <- case typ of
            "embed-npub" -> do
                case bech32ToPubKeyXO processedMatchText of
                    Just profilePubKey -> do
                        profile <- getProfile profilePubKey
                        let profileDisplayName = fromMaybe (Text.take 8 processedMatchText <> "...") $
                                              getDisplayName profile
                            html = "<a href=\"profile://" <> processedMatchText <>
                                   "\" style=\"color: #9C27B0\">@" <> profileDisplayName <> "</a>"
                        pure [["text", html]]
                    Nothing ->
                        pure [["text", processedMatchText]]  -- Invalid npub

            "embed-nprofile" -> do
                case nprofileToPubKeyXO processedMatchText of
                    Just (profilePubKey, _) -> do
                        profile <- getProfile profilePubKey
                        let profileDisplayName = fromMaybe (Text.take 8 processedMatchText <> "...") $
                                              getDisplayName profile
                            html = "<a href=\"profile://" <> processedMatchText <>
                                   "\" style=\"color: #9C27B0\">@" <> profileDisplayName <> "</a>"
                        pure [["text", html]]
                    Nothing ->
                        pure [["text", processedMatchText]]  -- Invalid nprofile

            "embed-url" -> do
                let html = "<a href=\"" <> processedMatchText <>
                           "\" style=\"color: #9C27B0\">" <> processedMatchText <> "</a>"
                pure [["text", html]]

            _ -> pure [[typ, processedMatchText]]  -- All other types as regular components

        -- Process the rest of the content
        restParts <- processContentWithMatchesM text end rest
        pure $ beforePart ++ matchPart ++ restParts

    -- Helper to get display name from profile
    getDisplayName :: Profile -> Maybe Text
    getDisplayName profile =
        -- Use displayName or name, whichever is available
        case displayName profile of
            Just dn | not (Text.null dn) -> Just dn
            _ -> case name profile of
                   Just n | not (Text.null n) -> Just n
                   _ -> Nothing

    -- Merge adjacent text parts - non-effectful
    mergeAdjacentTextParts :: [[Text]] -> [[Text]]
    mergeAdjacentTextParts [] = []
    mergeAdjacentTextParts [x] = [x]
    mergeAdjacentTextParts (part1:part2:rest) =
        case (part1, part2) of
            (["text", content1], ["text", content2]) ->
                -- Merge adjacent text parts
                mergeAdjacentTextParts ([["text", content1 <> content2]] ++ rest)
            _ -> part1 : mergeAdjacentTextParts (part2:rest)

    -- Helper to replace newlines with HTML breaks
    replaceNewlines :: Text -> Text
    replaceNewlines = Text.replace "\n" "<br>"

-- Helper function to find an available filename
findAvailableFilename :: (FileSystem :> es) => FilePath -> FilePath -> Eff es FilePath
findAvailableFilename dir baseFileName = do
  let tryPath = dir </> baseFileName
  exists <- doesFileExist tryPath
  if not exists
    then return tryPath
    else findNextAvailable dir baseFileName 1
  where
    findNextAvailable :: (FileSystem :> es) => FilePath -> FilePath -> Int -> Eff es FilePath
    findNextAvailable dir' fileName counter = do
      let ext = takeExtension fileName
          baseName = dropExtension fileName
          newName = baseName ++ " (" ++ show counter ++ ")" ++ ext
          tryPath = dir' </> newName
      exists <- doesFileExist tryPath
      if not exists
        then return tryPath
        else findNextAvailable dir' fileName (counter + 1)
