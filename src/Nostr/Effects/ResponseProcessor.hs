{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nostr.Effects.ResponseProcessor where

import Control.Monad (forever)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State)
import Effectful.TH

import Nostr.Effects.Logging
import Nostr.Types

type AppState = () -- @todo dummy

data ResponseProcessor :: Effect where
    ProcessResponses :: TQueue Response -> ResponseProcessor m ()

type instance DispatchOf ResponseProcessor = Dynamic

makeEffect ''ResponseProcessor

runResponseProcessor
  :: forall (es :: [Effect]) a.
     (Concurrent :> es, Logging :> es, State AppState :> es)
  => Eff (ResponseProcessor : es) a
  -> Eff es a
runResponseProcessor = interpret $ \_ -> \case
    ProcessResponses queue -> forever $ do
        msgs <- atomically $ flushTQueue queue
        logDebug $ T.pack $ "Processing incoming data: " ++ show msgs
        threadDelay $ 100 * 1000 -- to avoid re-rendering, we only send 10 times per second new data in batches to the UI
        -- @todo move data to app state
