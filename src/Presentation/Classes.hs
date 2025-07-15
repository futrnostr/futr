module Presentation.Classes where

import Control.Monad (void)
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), defConfig, keyOrder)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.FileSystem
import Effectful.State.Static.Shared (State, get, gets,modify)
import Graphics.QML
import System.FilePath ((</>), takeExtension, dropExtension)

import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (Event(..), EventId(..), Kind(..), Rumor(..), eventIdFromHex)
import Nostr.Event qualified as NE
import Nostr.Keys (PubKeyXO, exportPubKeyXO, keyPairToPubKeyXO)
import Nostr.Profile (Profile(..))
import Nostr.ProfileManager (ProfileManager, getProfile)
import Nostr.Util (Util, formatDateTime, getKeyPair)
import QtQuick (QtQuick, PropertyMap(..), PropertyName, QtQuickState(..), UIReferences(..), storeProfileContentRef, notify, emptyUpdates, UIUpdates(..))
import Store.Lmdb (LmdbStore, getCommentsWithIndentationLevel, getEvent, getEventRelays, getFollows)
import Types (AppState(..), Feed(..), Follow(..), Language(..), Post(..), PublishStatus(..), RelayPool(..))
import Downloader (Downloader(..), DownloadStatus(..), hasDownload, download, peekMimeType)


-- Effect for creating C++ classes for usage in QtQuick QML
data Classes :: Effect where
    FeedClass :: FactoryPool EventId -> SignalKey (IO ()) -> Classes m (Class ())
    ProfileClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    PostClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    PublishStatusClass :: SignalKey (IO ()) -> Classes m (Class EventId)
    FollowClass :: SignalKey (IO ()) -> Classes m (Class PubKeyXO)
    CommentClass :: SignalKey (IO ()) -> Classes m (Class (EventId, Int))


type instance DispatchOf Classes = Dynamic


feedClass :: Classes :> es => FactoryPool EventId -> SignalKey (IO ()) -> Eff es (Class ())
feedClass postsPool changeKey = send $ FeedClass postsPool changeKey

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
  , Downloader :> es
  , Concurrent :> es
  , Nostr :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Helper to load a post from the current feed
loadPostFromFeed :: ClassesEff es => EventId -> Eff es (Maybe Post)
loadPostFromFeed eid = do
    feed <- gets @AppState currentFeed
    case feed of
        Just feed' -> case Map.lookup eid (feedEventMap feed') of
            Just post -> return $ Just post
            Nothing -> do
                mp <- loadPost eid
                case mp of
                    Just p -> do
                        updateFeed eid p
                        return $ Just p
                    Nothing -> return Nothing
        Nothing -> do
            mp <- loadPost eid
            case mp of
                Just p -> do
                    updateFeed eid p
                    return $ Just p
                Nothing -> return Nothing
    where
        loadPost :: ClassesEff es => EventId -> Eff es (Maybe Post)
        loadPost eid' = do
            mev <- getEvent eid'
            case mev of
                Just ev -> do
                    post <- createPost ev
                    return $ Just $ post
                Nothing -> return Nothing

        updateFeed :: ClassesEff es => EventId -> Post -> Eff es ()
        updateFeed eid' p = modify @AppState $ \st -> do
            let feed' = fromMaybe (error "No feed") $ currentFeed st
            st { currentFeed = Just $ feed' { feedEventMap = Map.insert eid' p (feedEventMap feed') } }

-- | Handler for subscription effects.
runClasses
  :: ClassesEff es
  => Eff (Classes : es) a
  -> Eff es a
runClasses = interpret $ \_ -> \case
    FeedClass postsPool changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "events" changeKey' $ \_ -> do
                cf <- runE $ gets @AppState currentFeed
                case cf of
                  Just feed -> mapM (getPoolObject postsPool) [postId post | post <- feedEvents feed]
                  Nothing -> return []
            ]

    ProfileClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertySigRO' "id" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                let value = TE.decodeUtf8 $ B16.encode $ exportPubKeyXO pk
                return value,

            defPropertySigRO' "npub" changeKey' $ \obj -> do
                let pk = fromObjRef obj :: PubKeyXO
                return $ pubKeyXOToBech32 pk,

            defPropertySigRO' "name" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "name" obj
                (profile, _) <- getProfile pk
                pure $ name profile,

            defPropertySigRO' "displayName" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "displayName" obj
                (profile, _) <- getProfile pk
                pure $ displayName profile,

            defPropertySigRO' "about" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "about" obj
                (profile, _) <- getProfile pk
                pure $ about profile,

            defPropertySigRO' "picture" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "picture" obj
                (profile, _) <- getProfile pk
                resolveProfileImage (const $ pure $ picture profile) pk (Just obj),

            defPropertySigRO' "nip05" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "nip05" obj
                (profile, _) <- getProfile pk
                pure $ nip05 profile,

            defPropertySigRO' "banner" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "banner" obj
                (profile, _) <- getProfile pk
                resolveProfileImage (const $ pure $ banner profile) pk (Just obj),

            defPropertySigRO' "isFollow" changeKey' $ \obj -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                storeProfileObjRef pk "isFollow" obj
                kp <- getKeyPair
                let currentPubKey = keyPairToPubKeyXO kp
                follows <- getFollows currentPubKey
                pure $ pk `elem` map pubkey follows,

            defPropertySigRO' "followerCount" changeKey' $ \_ -> do
                return (0 :: Int),
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowers pk,
                -}

            defPropertySigRO' "followingCount" changeKey' $ \_ -> do
                return (0 :: Int),
                {-
                st <- runE $ get @AppState
                let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
                runE $ getProfileEventCount subscribeToFollowing pk
                -}

            defMethod' "getProfilePicture" $ \obj pictureUrl -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                let url' _ = case pictureUrl of
                        Nothing -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just "" -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just url -> pure $ Just url
                resolveProfileImage url' pk (Just obj)
            ]


    PostClass changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        newClass [
            defPropertyRO' "id" $ \obj -> do
              let eid = fromObjRef obj :: EventId
              let value = TE.decodeUtf8 $ B16.encode $ getEventId eid
              return value,

            defPropertyRO' "nevent" $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just post' -> return $ eventToNevent (postEvent post') []
                Nothing -> return "",

            defPropertySigRO' "raw" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just p -> do
                  let e = postEvent p
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
              runE $ storePostObjRef eid "relays" obj
              relaysSet <- runE $ getEventRelays eid
              return $ Set.toList relaysSet,

            defPropertySigRO' "postType" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just p -> return $ postType p
                Nothing -> return "unknown",

            defPropertySigRO' "contentParts" changeKey' $ \obj -> runE $ do
              let eid = fromObjRef obj :: EventId
              storePostObjRef eid "contentParts" obj
              postMaybe <- loadPostFromFeed eid
              case postMaybe of
                Just post -> parseContentParts obj (postContent post)
                Nothing -> pure [],

            defPropertySigRO' "timestamp" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just post -> return $ postTimestamp post
                Nothing -> return "",

            defPropertySigRO' "authorId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just post -> return $ Just $ pubKeyXOToBech32 $ postAuthor post
                Nothing -> return Nothing,

            defPropertySigRO' "referencedPostId" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              postMaybe <- runE $ loadPostFromFeed eid
              case postMaybe of
                Just post-> case referencedPostId post of
                  Just eid' -> return $ Just $ eventIdToNote eid'
                  Nothing -> return Nothing
                Nothing -> return Nothing,

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
                runE $ resolveProfileImage (const $ pure $ picture profile) pk (Just obj)),
            defMethod' "getProfilePicture" $ \obj pictureUrl -> runE $ do
                let pk = fromObjRef obj :: PubKeyXO
                let url' _ = case pictureUrl of
                        Nothing -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just "" -> pure $ Just $ "https://robohash.org/" <> pubKeyXOToBech32 pk <> ".png?size=50x50"
                        Just url -> pure $ Just url
                resolveProfileImage url' pk (Just obj)
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
        let httpMatches = findPrefixMatches text "http://"
            httpsMatches = findPrefixMatches text "https://"
            nostrMatches = findNostrMatches text
        in httpMatches ++ httpsMatches ++ nostrMatches

    -- Find all occurrences of a prefix and extract the full entity
    findPrefixMatches :: Text -> Text -> [(Int, Int, Text)]
    findPrefixMatches text prefix =
        let positions = Text.breakOnAll prefix text
        in concatMap (\(before, _) ->
                let startPos = Text.length before
                    fullEntity = extractEntity (Text.drop (Text.length before) text)
                    endPos = startPos + Text.length fullEntity
                in [(startPos, endPos, "embed-url")]
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
                let url = processedMatchText
                status <- hasDownload url
                case status of
                  Ready (cacheFile, mime, _) | "image/" `Text.isPrefixOf` mime ->
                    pure [["image", "file:///" <> Text.pack cacheFile, url]]
                  Ready (cacheFile, mime, _) | "video/" `Text.isPrefixOf` mime ->
                    pure [["video", "file:///" <> Text.pack cacheFile, url]]
                  _ -> do
                    let html = "<a href=\"" <> url <> "\" style=\"color: #9C27B0\">" <> url <> "</a>"
                    void $ async $ do
                      mimeResult <- peekMimeType url
                      case mimeResult of
                        Right mime | "image/" `Text.isPrefixOf` mime || "video/" `Text.isPrefixOf` mime -> do
                              void $ download url
                              notify $ emptyUpdates { contentObjectsToSignal = [objRef] }
                        _ -> pure ()
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


-- | Helper to resolve a remote image URL to a local file if downloaded, otherwise trigger async download and signal.
resolveProfileImage
  :: ClassesEff es
  => (PubKeyXO -> Eff es (Maybe Text))
  -> PubKeyXO
  -> Maybe (ObjRef PubKeyXO)
  -> Eff es Text
resolveProfileImage getField pk mObj = do
  mUrl <- getField pk
  case mUrl of
    Just url | "http://" `Text.isPrefixOf` url || "https://" `Text.isPrefixOf` url -> do
      status <- hasDownload url
      case status of
        Ready (cacheFile, mime, _) | "image/" `Text.isPrefixOf` mime ->
          pure $ "file:///" <> Text.pack cacheFile
        _ -> do
          void $ async $ do
            mimeResult <- peekMimeType url
            case mimeResult of
              Right mime | "image/" `Text.isPrefixOf` mime -> do
                void $ download url
                case mObj of
                  Just obj -> notify $ emptyUpdates { profileObjectsToSignal = [obj] }
                  Nothing -> pure ()
              _ -> pure ()
          pure url
    _ -> pure $ fromMaybe "" mUrl


-- | Create a post from an event.
-- @todo code duplication with Futr.hs
createPost :: ClassesEff es => Event -> Eff es Post
createPost ev = do
  (authorPubKey, content', ts) <- case kind ev of
      NE.GiftWrap -> do
        kp <- getKeyPair
        sealed <- unwrapGiftWrap ev kp
        rumor <- maybe (return Nothing) (unwrapSeal `flip` kp) sealed
        return $ (fromMaybe (pubKey ev) (rumorPubKey <$> rumor), fromMaybe "" $ rumorContent <$> rumor, fromMaybe 0 $ rumorCreatedAt <$> rumor)
      NE.Repost -> do
        let repostedId = listToMaybe [ fromMaybe (error "Invalid event ID") $ eventIdFromHex eidStr | ("e":eidStr:_) <- tags ev ]
        case repostedId of
            Just eid' -> do
              return $ (pubKey ev, "nostr:" <> eventIdToNote eid', createdAt ev)
            Nothing -> return (pubKey ev, "", createdAt ev)
      _ -> return (pubKey ev, content ev, createdAt ev)
  formattedTime <- formatDateTime English ts
  pure Post
    { postId = eventId ev
    , postAuthor = authorPubKey
    , postEvent = ev
    , postContent = content'
    , postTimestamp = formattedTime
    , postType = case kind ev of
        NE.ShortTextNote ->
          if any (\t -> case t of
                        ("q":_) -> True
                        _ -> False) (tags ev)
          then "quote_repost"
          else "short_text_note"
        NE.Repost -> "repost"
        NE.Comment -> "comment"
        NE.GiftWrap -> "gift_wrap"
        _ -> "unknown"
    , referencedPostId = case kind ev of
        NE.Repost -> listToMaybe [ fromMaybe (error "Invalid event ID") $ eventIdFromHex eidStr | ("e":eidStr:_) <- tags ev ]
        _ -> Nothing
    }
