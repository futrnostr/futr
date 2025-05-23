module Presentation.Classes where

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
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, modify)
import Graphics.QML
import System.FilePath ((</>), takeExtension, dropExtension)

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), Kind(..), Rumor(..), eventIdFromHex)
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (ProfileManager, getProfile)
import Nostr.Util (Util, getCurrentTime, getKeyPair)
import QtQuick (QtQuick, PropertyMap(..), PropertyName, QtQuickState(..), UIReferences(..), storeProfileContentRef)
import Store.Lmdb (LmdbStore, getCommentsWithIndentationLevel, getEvent, getEventRelays, getFollows)
import TimeFormatter
import Types (AppState(..), Follow(..), PublishStatus(..), RelayPool(..))


-- Effect for creating C++ classes for usage in QtQuick QML
data Classes :: Effect where
    ProfileClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    PostClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    PublishStatusClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    FollowClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    CommentClass :: SignalKey (IO ()) -> Classes m (Class (EventId, Int))


type instance DispatchOf Classes = Dynamic


profileClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class PubKeyXO)
profileClass changeKey = send $ ProfileClass changeKey

postClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class EventId)
postClass changeKey = send $ PostClass changeKey

publishStatusClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class EventId)
publishStatusClass changeKey = send $ PublishStatusClass changeKey

followClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class PubKeyXO)
followClass changeKey = send $ FollowClass changeKey

commentClass :: Classes :> es => SignalKey (IO ()) -> Eff es (Class (EventId, Int))
commentClass changeKey = send $ CommentClass changeKey


-- | SubscriptionEff
type ClassesEff es =
  ( QtQuick :> es
  , State QtQuickState :> es
  , State RelayPool :> es
  , State AppState :> es
  , ProfileManager :> es
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
                runE $ storeProfileObjRef pk "name" obj
                (profile, _) <- runE $ getProfile pk
                return $ name profile,

            defPropertySigRO' "displayName" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "displayName" obj
                (profile, _) <- runE $ getProfile pk
                return $ displayName profile,

            defPropertySigRO' "about" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "about" obj
                (profile, _) <- runE $ getProfile pk
                return $ about profile,

            defPropertySigRO' "picture" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "picture" obj
                (profile, _) <- runE $ getProfile pk
                return $ picture profile,

            defPropertySigRO' "nip05" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "nip05" obj
                (profile, _) <- runE $ getProfile pk
                return $ nip05 profile,

            defPropertySigRO' "banner" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "banner" obj
                (profile, _) <- runE $ getProfile pk
                return $ banner profile,

            defPropertySigRO' "isFollow" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                runE $ storeProfileObjRef pk "isFollow" obj
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


        newClass [
            defPropertyRO' "id" $ \obj -> do
              let eid = fromObjRef obj :: EventId
              let value = TE.decodeUtf8 $ B16.encode $ getEventId eid
              return value,

            defPropertyRO' "nevent" $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just e -> do
                  relaysSet <- runE $ getEventRelays eid
                  let r = if Set.null relaysSet then "" else head $ Set.toList relaysSet
                  return $ eventToNevent e [r]
                Nothing -> return "",

            defPropertySigRO' "raw" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just e -> do
                  let prettyConfig = defConfig {
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
              runE $ storePostObjRef eid "relays" obj
              relaysSet <- runE $ getEventRelays eid
              return $ Set.toList relaysSet,

            defPropertySigRO' "postType" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              let value = case eventMaybe of
                    Just e ->
                        pack $ case kind e of
                            ShortTextNote ->
                                if any (\t -> case t of
                                              ("q":_) -> True
                                              _ -> False) (tags e)
                                then "quote_repost"
                                else "short_text_note"
                            Repost -> "repost"
                            Comment -> "comment"
                            GiftWrap -> "gift_wrap"
                            _ -> "unknown"
                    Nothing -> "unknown"
              return value,

            defPropertySigRO' "contentParts" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              runE $ storePostObjRef eid "contentParts" obj
              value <- runE $ getEvent eid >>= \case
                Just ev -> do
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
                  parseContentParts obj content'
                Nothing -> return []
              return value,

            defPropertySigRO' "timestamp" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              value <- case eventMaybe of
                Just e -> do
                  now <- runE getCurrentTime
                  return $ Just $ formatDateTime English now (createdAt e)
                Nothing -> return Nothing
              return value,

            defPropertySigRO' "authorId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              runE $ getEvent eid >>= \case
                Just ev -> do
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
                Just e -> runE $ getRootReference e >>= \case
                  Just refId -> return $ Just $ eventIdToNote refId
                  Nothing -> return Nothing
                Nothing -> return Nothing,

            defPropertySigRO' "parent" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just e -> do
                  parentId <- runE $ getParentReference e
                  rootId <- runE $ getRootReference e
                  case (parentId, rootId) of
                    (Just p, Just r) | p /= r -> return $ Just $ eventIdToNote p
                    _ -> return Nothing
                Nothing -> return Nothing,

            defPropertySigRO' "referencedPostId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              eventMaybe <- runE $ getEvent eid
              case eventMaybe of
                Just e | kind e == Repost -> do
                  let tags' = tags e
                      repostedId = listToMaybe [eidStr | ("e":eidStr:_) <- tags']
                  return repostedId
                _ -> return Nothing,

            defPropertySigRO' "repostCount" changeKey' $ \_ -> do
                return (0 :: Int),

            defPropertySigRO' "comments" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              runE $ storePostObjRef eid "comments" obj
              runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { currentPostCommentsObjRef = Just obj } }
              commentIds <- runE $ getCommentsWithIndentationLevel eid
              commentClass' <- runE $ createCommentClass changeKey'
              mapM (newObject commentClass') commentIds
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
              accessor obj followData

        newClass [
            followProp "pubkey" $ \_ -> return . maybe "" (pubKeyXOToBech32 . pubkey),
            followProp "petname" $ \_ -> return . maybe "" (fromMaybe "" . petName),
            followProp "displayName" $ \obj -> maybe (return "") (\follow -> do
                let pk = pubkey follow
                runE $ storeProfileObjRef pk "displayName" obj
                (profile, _) <- runE $ getProfile pk
                return $ fromMaybe "" (displayName profile)),
            followProp "name" $ \obj -> maybe (return "") (\follow -> do
                let pk = pubkey follow
                runE $ storeProfileObjRef pk "name" obj
                (profile, _) <- runE $ getProfile pk
                return $ fromMaybe "" (name profile)),
            followProp "picture" $ \obj -> maybe (return "") (\follow -> do
                let pk = pubkey follow
                runE $ storeProfileObjRef pk "picture" obj
                (profile, _) <- runE $ getProfile pk
                return $ fromMaybe "" (picture profile))
          ]

    CommentClass changeKey' -> createCommentClass changeKey'



createCommentClass :: ClassesEff es => SignalKey (IO ()) -> Eff es (Class (EventId, Int))
createCommentClass changeKey' = liftIO $ do
    newClass [
        defPropertySigRO' "post" changeKey' $ \obj -> do
            let (eid, _) = fromObjRef obj :: (EventId, Int)
            return $ TE.decodeUtf8 $ B16.encode $ getEventId eid,

        defPropertySigRO' "indentationLevel" changeKey' $ \obj -> do
            let (_, level) = fromObjRef obj :: (EventId, Int)
            return level
        ]


-- | Store a profile object reference in the property map
storeProfileObjRef :: ClassesEff es => PubKeyXO -> PropertyName -> ObjRef PubKeyXO -> Eff es ()
storeProfileObjRef pk propName obj = withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    weakObjRef <- toWeakObjRef obj

    runE $ modify @QtQuickState $ \st ->
        let currentMap = profileObjRefs (propertyMap st)
            updatedMap = Map.alter (Just . maybe 
                                         (Map.singleton propName weakObjRef)
                                         (Map.insert propName weakObjRef)) 
                                 pk 
                                 currentMap
        in st { propertyMap = (propertyMap st) { profileObjRefs = updatedMap } }

    finalizer <- newObjFinaliser $ \_ -> do
        runE $ modify @QtQuickState $ \st' ->
            let currentMap = profileObjRefs (propertyMap st')
                updatedMap = Map.delete pk currentMap
            in st' { propertyMap = (propertyMap st') { profileObjRefs = updatedMap } }


    addObjFinaliser finalizer obj

-- | Store a post object reference in the property map
storePostObjRef :: ClassesEff es => EventId -> PropertyName -> ObjRef EventId -> Eff es ()
storePostObjRef evId propName obj = withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    weakObjRef <- toWeakObjRef obj

    runE $ modify @QtQuickState $ \st ->
        let currentMap = postObjRefs (propertyMap st)
            updatedMap = Map.alter (Just . maybe 
                                         (Map.singleton propName weakObjRef)
                                         (Map.insert propName weakObjRef)) 
                                 evId 
                                 currentMap
        in st { propertyMap = (propertyMap st) { postObjRefs = updatedMap } }

    finalizer <- newObjFinaliser $ \_ -> do
        runE $ modify @QtQuickState $ \st' ->
            let currentMap = postObjRefs (propertyMap st')
                updatedMap = Map.delete evId currentMap
            in st' { propertyMap = (propertyMap st') { postObjRefs = updatedMap } }


    addObjFinaliser finalizer obj


-- | Parse content into parts (text, images, URLs, and nostr references)
parseContentParts :: ClassesEff es => ObjRef EventId -> Text -> Eff es [[Text]]
parseContentParts objRef contentText
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
        in concatMap (\(before, after) ->
                let startPos = Text.length before
                    fullEntity = extractEntity after
                    endPos = startPos + Text.length fullEntity
                    -- Only match if it's a valid nostr reference
                    isValidNostrRef = Text.length fullEntity >= 11 &&
                        let prefix = Text.take 5 (Text.drop 6 fullEntity) -- after "nostr:"
                        in prefix `elem` ["note1", "npub1", "nprof", "neven", "naddr"]
                    -- Determine the nostr reference type
                    nostrType = if isValidNostrRef
                                then
                                    let prefix = Text.take 5 (Text.drop 6 fullEntity) -- after "nostr:"
                                    in case prefix of
                                        "note1" -> "note"
                                        "npub1" -> "embed-npub"
                                        "nprof" -> "embed-nprofile"
                                        "neven" -> "nevent"
                                        "naddr" -> "naddr"
                                        _ -> "nostr"
                                else "text" -- Treat as regular text if not a valid nostr reference
                in if isValidNostrRef
                   then [(startPos, endPos, nostrType)]
                   else [] -- Skip invalid matches
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
    processContentWithMatchesM :: ClassesEff es => Text -> Int -> [(Int, Int, Text)] -> Eff es [[Text]]
    processContentWithMatchesM text currentPos [] = do
        let remaining = Text.drop currentPos text
        pure $ if Text.null remaining
               then []
               else [["text", replaceNewlines remaining]]

    processContentWithMatchesM text currentPos ((start, end, matchType):rest) = do
        let beforeMatch = Text.take (start - currentPos) $ Text.drop currentPos text
            matchText = Text.take (end - start) $ Text.drop start text
            processedMatchText = if "nostr:" `Text.isPrefixOf` matchText &&
                                  matchType `elem` ["note", "embed-npub", "embed-nprofile", "nevent", "naddr"]
                               then Text.drop 6 matchText
                               else Text.strip matchText

        beforeParts <- if Text.null beforeMatch
                      then pure []
                      else pure [["text", replaceNewlines beforeMatch]]

        matchParts <- case matchType of
            "embed-npub" -> do
                case bech32ToPubKeyXO processedMatchText of
                    Just profilePubKey -> do
                        (profile, _) <- getProfile profilePubKey
                        storeProfileContentRef profilePubKey objRef
                        let profileDisplayName = fromMaybe (Text.take 8 processedMatchText <> "...") $
                                              getDisplayName profile
                            html = "<a href=\"profile://" <> processedMatchText <>
                                  "\" style=\"color: #9C27B0\">@" <> profileDisplayName <> "</a>"
                        pure [["text", html]]
                    Nothing -> pure [["text", matchText]]

            "embed-nprofile" -> do
                case nprofileToPubKeyXO processedMatchText of
                    Just (profilePubKey, _) -> do
                        (profile, _) <- getProfile profilePubKey
                        storeProfileContentRef profilePubKey objRef
                        let profileDisplayName = fromMaybe (Text.take 8 processedMatchText <> "...") $
                                              getDisplayName profile
                            html = "<a href=\"profile://" <> processedMatchText <>
                                  "\" style=\"color: #9C27B0\">@" <> profileDisplayName <> "</a>"
                        pure [["text", html]]
                    Nothing -> pure [["text", matchText]]

            "embed-url" -> do
                let html = "<a href=\"" <> processedMatchText <>
                          "\" style=\"color: #9C27B0\">" <> processedMatchText <> "</a>"
                pure [["text", html]]

            _ -> pure [[matchType, processedMatchText]]

        restParts <- processContentWithMatchesM text end rest
        pure $ beforeParts ++ matchParts ++ restParts

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
