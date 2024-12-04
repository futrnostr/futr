module Nostr.EventStore where

import Database.RocksDB
import qualified Data.ByteString.Lazy as LBS
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory
  )
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.TH (makeEffect)
import Data.Aeson (encode, decode)
import Data.Text (unpack)
import System.FilePath ((</>))

import Nostr.Keys (PubKeyXO)
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Types (Event(..), EventId(..))
import Types (AppState(..))


-- | EventStore effects
data EventStore :: Effect where
    PutEvent :: Event -> EventStore m ()
    GetEvent :: EventId -> EventStore m (Maybe Event)
    InitEventDB :: PubKeyXO -> EventStore m ()

-- | Dispatch type for EventStore effect.
type instance DispatchOf EventStore = Dynamic

makeEffect ''EventStore

-- | Effectful type for EventStore.
type EventStoreEff es = (IOE :> es, FileSystem :> es, State AppState :> es)


-- | Run EventStore operations with an existing DB handle
runEventStore :: EventStoreEff es => Eff (EventStore : es) a -> Eff es a
runEventStore = interpret $ \_ -> \case
    PutEvent event -> do
        st <- State.get
        case eventDb st of
            Just db -> liftIO $ put db (getEventId $ eventId event) (LBS.toStrict $ encode event)
            Nothing -> error "Database not initialized"

    GetEvent eid -> do
        st <- State.get
        case eventDb st of
            Just db -> do
                result <- liftIO $ get db (getEventId eid)
                return $ result >>= (decode . LBS.fromStrict)
            Nothing -> error "Database not initialized"

    InitEventDB pk -> do
        baseDir <- getXdgDirectory XdgData ("futrnostr" </> unpack (pubKeyXOToBech32 pk))
        createDirectoryIfMissing True baseDir

        let dbConfig = Config
              { createIfMissing = True
              , errorIfExists = False
              , paranoidChecks = False
              , maxFiles = Nothing
              , prefixLength = Nothing
              , bloomFilter = True
              }

        db <- liftIO $ withDB (baseDir </> "events.db") dbConfig pure
        State.modify $ \st -> st { eventDb = Just db }
