{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Downloader (
    -- Effect
    Downloader(..),
    runDownloader,
    -- Functions
    hasDownload,
    download,
    peekMimeType,
    clearCache,
    -- State
    DownloaderState(..),
    initialDownloaderState,
    DownloadStatus(..),
    DownloadError(..)
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import System.FilePath ((</>), takeExtension)
import System.Directory (doesFileExist, removeFile, listDirectory)
import Control.Exception (try, SomeException)
import Control.Monad (void, when, forM_)
import Network.Wreq (headWith, getWith, responseHeader, responseBody, defaults, responseHeaders)
import qualified Network.Wreq as Wreq
import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.FileSystem (FileSystem, getXdgDirectory, XdgDirectory(XdgData), createDirectoryIfMissing)
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Util (Util, getKeyPair)
import Control.Lens ((^.))
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Date as HTTPDate
import qualified Data.Text.Encoding as TE

-- | Error type for download failures
newtype DownloadError = DownloadError Text deriving (Show, Eq)

-- | Download status for a given URL
--   No STM/TMVar, just pure state
--   UI can poll for status or be notified via signal
--
data DownloadStatus
  = NotStarted
  | Downloading
  | Ready (FilePath, Text, UTCTime)
  | Failed DownloadError
  deriving (Show, Eq)

-- | Downloader state: tracks download status for each URL
--   No STM, just pure state
--
data DownloaderState = DownloaderState
  { inProgress :: Map.Map Text DownloadStatus
  }

-- | Initial empty downloader state
initialDownloaderState :: DownloaderState
initialDownloaderState = DownloaderState { inProgress = Map.empty }

-- | Downloader effect
--   HasDownload: check if file is cached and valid (returns Ready or NotStarted/Failed)
--   Download: ensure file is cached (deduplicates in-progress downloads, starts async if needed)
--   PeekMimeType: perform HEAD request and return Content-Type
--   ClearCache: remove expired files
--
data Downloader :: Effect where
  HasDownload :: Text -> Downloader m DownloadStatus
  Download :: Text -> Downloader m DownloadStatus
  PeekMimeType :: Text -> Downloader m (Either DownloadError Text)
  ClearCache :: Downloader m ()

type instance DispatchOf Downloader = Dynamic

-- | Check if a download is available in the cache or in progress
hasDownload :: Downloader :> es => Text -> Eff es DownloadStatus
hasDownload url = send $ HasDownload url

-- | Start a download (if not already in progress or cached)
download :: Downloader :> es => Text -> Eff es DownloadStatus
download url = send $ Download url

-- | Peek the MIME type of a URL (HEAD request)
peekMimeType :: Downloader :> es => Text -> Eff es (Either DownloadError Text)
peekMimeType url = send $ PeekMimeType url

-- | Clear expired files from the cache
clearCache :: Downloader :> es => Eff es ()
clearCache = send ClearCache

runDownloader :: (State DownloaderState :> es, Util :> es, FileSystem :> es, IOE :> es, Concurrent :> es) => Eff (Downloader : es) a -> Eff es a
runDownloader = interpret $ \_ -> \case
  HasDownload url -> do
    st <- get
    case Map.lookup url (inProgress st) of
      Just status@(Ready _) -> pure status
      Just status@(Failed _)    -> pure status
      _ -> do
        -- Check cache on disk
        cacheDir <- getCacheDirForCurrentUser
        let cacheFile = cacheDir </> urlToCacheFile url
            metaFile  = cacheFile ++ ".meta"
        exists <- liftIO $ doesFileExist cacheFile
        metaExists <- liftIO $ doesFileExist metaFile
        if not (exists && metaExists)
          then pure NotStarted
          else do
            meta <- liftIO $ readMetaFile metaFile
            now <- liftIO getCurrentTime
            case meta of
              Just (mime, expiry) | expiry > now -> pure $ Ready (cacheFile, mime, expiry)
              _ -> pure NotStarted

  Download url -> do
    st <- get
    case Map.lookup url (inProgress st) of
      Just status@(Ready _) -> pure status
      Just status@Downloading   -> pure status
      Just status@(Failed _)    -> pure status
      _ -> do
        -- Set to Downloading and start async job
        modify $ \s -> s { inProgress = Map.insert url Downloading (inProgress s) }
        void $ async $ do
          now <- liftIO getCurrentTime
          result <- downloadAndCache url now
          case result of
            Right (cacheFile, mime, expiry) ->
              modify $ \s -> s { inProgress = Map.insert url (Ready (cacheFile, mime, expiry)) (inProgress s) }
            Left err ->
              modify $ \s -> s { inProgress = Map.insert url (Failed err) (inProgress s) }
        pure Downloading

  PeekMimeType url -> do
    -- Perform HEAD request and extract Content-Type
    headResult <- liftIO $ try $ headWith defaults (T.unpack url)
    case headResult of
      Left (e :: SomeException) -> pure $ Left $ DownloadError (T.pack $ show e)
      Right resp ->
        let mime = T.pack $ BS.unpack $ resp ^. responseHeader "Content-Type"
        in pure $ Right mime

  ClearCache -> do
    cacheDir <- getCacheDirForCurrentUser
    files <- liftIO $ listDirectory cacheDir
    now <- liftIO getCurrentTime
    forM_ files $ \f ->
      when (takeExtension f == ".meta") $ do
        let metaFile = cacheDir </> f
            cacheFile = cacheDir </> takeBase f
        meta <- liftIO $ readMetaFile metaFile
        case meta of
          Just (_, expiry) | expiry < now -> do
            liftIO $ removeIfExists cacheFile
            liftIO $ removeIfExists metaFile
          _ -> pure ()

-- | Download and cache a URL, returning (FilePath, mime, expiry) or error
--   Uses HEAD to get mime and expiry, GET to download
--   Caches file and .meta file
--   Returns error on failure
downloadAndCache :: (Util :> es, FileSystem :> es, IOE :> es) => Text -> UTCTime -> Eff es (Either DownloadError (FilePath, Text, UTCTime))
downloadAndCache url now = do
  cacheDir <- getCacheDirForCurrentUser
  let cacheFile = cacheDir </> urlToCacheFile url
      metaFile  = cacheFile ++ ".meta"
  createDirectoryIfMissing True cacheDir
  -- HEAD request for mime and expiry
  headResult <- liftIO $ try $ headWith defaults (T.unpack url)
  case headResult of
    Left (e :: SomeException) -> pure $ Left $ DownloadError (T.pack $ show e)
    Right resp -> do
      let mime = T.pack $ BS.unpack $ resp ^. responseHeader "Content-Type"
          expiry = parseExpiry resp now
      -- GET request for file
      getResult <- liftIO $ try $ getWith defaults (T.unpack url)
      case getResult of
        Left (e :: SomeException) -> pure $ Left $ DownloadError (T.pack $ show e)
        Right getResp -> do
          let body = getResp ^. responseBody
          liftIO $ BL.writeFile cacheFile body
          liftIO $ writeMetaFile metaFile mime expiry
          pure $ Right (cacheFile, mime, expiry)

-- | Get the per-user cache directory for media downloads
getCacheDirForCurrentUser :: (Util :> es, FileSystem :> es, IOE :> es) => Eff es FilePath
getCacheDirForCurrentUser = do
  kp <- getKeyPair
  let pk = keyPairToPubKeyXO kp
      npub = pubKeyXOToBech32 pk
  baseDir <- getXdgDirectory XdgData ("futrnostr" </> T.unpack npub)
  pure $ baseDir </> "media-cache"

-- | Hash the URL to a cache filename
urlToCacheFile :: Text -> FilePath
urlToCacheFile url = BS.unpack $ B16.encode $ SHA256.hash $ BS.pack $ T.unpack url

-- | Remove file if it exists
removeIfExists :: FilePath -> IO ()
removeIfExists f = do
  exists <- doesFileExist f
  when exists $ removeFile f

-- | Default cache expiry in seconds (10 minutes)
defaultCacheSeconds :: Int
defaultCacheSeconds = 600  -- 10 minutes

-- | Parse expiry from HTTP headers, fallback to 10 minutes
parseExpiry :: Wreq.Response a -> UTCTime -> UTCTime
parseExpiry resp now =
  let cacheControl = lookupHeader "Cache-Control" resp
      expires = lookupHeader "Expires" resp
  in case (cacheControl, expires) of
    (Just cc, _) | "max-age=" `T.isInfixOf` cc ->
      let secs = readInt $ T.dropWhile (not . (`elem` ['0'..'9'])) $ T.dropWhile (/= '=') cc
      in addUTCTime (fromIntegral $ fromMaybe defaultCacheSeconds secs) now
    (_, Just expStr) ->
      case parseHttpDate expStr of
        Just t -> t
        Nothing -> addUTCTime (fromIntegral defaultCacheSeconds) now
    _ -> addUTCTime (fromIntegral defaultCacheSeconds) now

-- | Read .meta file (mime, expiry)
readMetaFile :: FilePath -> IO (Maybe (Text, UTCTime))
readMetaFile f = do
  exists <- doesFileExist f
  if not exists then pure Nothing else do
    content <- readFile f
    let ls = lines content
    case ls of
      [mime, expiryStr] -> case reads expiryStr of
        [(t, _)] -> pure $ Just (T.pack mime, t)
        _ -> pure Nothing
      _ -> pure Nothing

-- | Write .meta file (mime, expiry)
writeMetaFile :: FilePath -> Text -> UTCTime -> IO ()
writeMetaFile f mime expiry =
  writeFile f $ T.unpack mime ++ "\n" ++ show expiry ++ "\n"

-- | Helper: take base filename (strip .meta)
takeBase :: FilePath -> FilePath
takeBase f = reverse $ drop 5 $ reverse f

-- | Helper: lookup header (case-insensitive)
lookupHeader :: BS.ByteString -> Wreq.Response a -> Maybe Text
lookupHeader h r = fmap (T.pack . BS.unpack) $ lookup (CI.mk h) (r ^. responseHeaders)

-- | Helper: parse HTTP date (RFC1123)
parseHttpDate :: Text -> Maybe UTCTime
parseHttpDate t =
  case HTTPDate.parseHTTPDate (TE.encodeUtf8 t) of
    Just d  -> Just (HTTPDate.httpDateToUTC d)
    Nothing -> Nothing

-- | Helper: readInt
readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
  [(i, _)] -> Just i
  _ -> Nothing
