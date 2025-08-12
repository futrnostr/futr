{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Presentation.RelayMgmtUI where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic (EffectHandler, interpret, send)
import Effectful.State.Static.Shared (State, get, modify)
import Graphics.QML hiding (fireSignal, runEngineLoop)

import QtQuick (QtQuickState(..), UIReferences(..))
import Logging
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Relay (RelayURI, getUri, isInboxCapable, isOutboxCapable)
import Nostr.Util
import RelayMgmt (RelayMgmt, addDMRelay, addGeneralRelay, removeDMRelay, removeGeneralRelay)
import Store.Lmdb (LmdbStore, getDMRelays, getGeneralRelays)
import Types (AppState(..), ConnectionState(..))
import Nostr.RelayPool (RelayPoolState(..), RelayData(..))


data RelayType = DMRelays | InboxRelays | OutboxRelays

-- | Relay Management UI Effect.
type RelayMgmgtUIEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , State QtQuickState :> es
  , RelayMgmt :> es
  , LmdbStore :> es
  , Logging :> es
  , Concurrent :> es
  , Util :> es
  , IOE :> es
  )

-- | Key Management Effect for creating QML UI.
data RelayMgmtUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> RelayMgmtUI m (ObjRef ())


-- | Dispatch for Key Management UI Effect.
type instance DispatchOf RelayMgmtUI = Dynamic


createUI :: RelayMgmtUI :> es => SignalKey (IO ()) -> Eff es (ObjRef ())
createUI changeKey = send $ CreateUI changeKey


-- | Run the Relay Management UI effect.
runRelayMgmtUI :: RelayMgmgtUIEff es => Eff (RelayMgmtUI : es) a -> Eff es a
runRelayMgmtUI action = interpret handleRelayMgmtUI action
  where
    handleRelayMgmtUI :: RelayMgmgtUIEff es => EffectHandler RelayMgmtUI es
    handleRelayMgmtUI _ = \case
      CreateUI changeKey -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        dmRelayClass <- newClass [
            defPropertySigRO' "url" changeKey $ \obj -> return $ fromObjRef obj,

            defPropertySigRO' "connectionState" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ getConnectionStateText uri' pst,

            defPropertySigRO' "connectionRetries" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> connectionAttempts rd
                Nothing -> 0,

            defPropertySigRO' "notices" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> notices rd
                Nothing -> []
            ,
            defPropertySigRO' "messagesReceived" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> messagesReceived rd
                Nothing -> 0
            ,
            defPropertySigRO' "eventsReceived" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> eventsReceived rd
                Nothing -> 0
            ,
            defPropertySigRO' "successCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> successCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "failureCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> failureCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "lastConnectedAt" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> maybe 0 id (lastConnectedAt rd)
                Nothing -> 0
            ,
            defPropertySigRO' "lastEoseAt" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> maybe 0 id (lastEoseAt rd)
                Nothing -> 0
            ,
            defPropertySigRO' "authRequiredCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> authRequiredCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "noticeCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> noticeCount rd
                Nothing -> 0
            ]

        relayClass <- newClass [
            defPropertySigRO' "url" changeKey $ \obj -> return $ fromObjRef obj,

            defPropertySigRO' "connectionState" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ getConnectionStateText uri' pst,

            defPropertySigRO' "isInbox" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              kp <- getKeyPair

              dmRelays <- getDMRelays (keyPairToPubKeyXO kp)
              let isInDMRelays = uri' `elem` dmRelays

              generalRelays <- getGeneralRelays (keyPairToPubKeyXO kp)
              let isInGeneralRelays = any (\r -> isInboxCapable r && getUri r == uri') generalRelays

              return $ isInDMRelays || isInGeneralRelays,

            defPropertySigRO' "isOutbox" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              kp <- getKeyPair

              dmRelays <- getDMRelays (keyPairToPubKeyXO kp)
              let isInDMRelays = uri' `elem` dmRelays

              generalRelays <- getGeneralRelays (keyPairToPubKeyXO kp)
              let isInGeneralRelays = any (\r -> isOutboxCapable r && getUri r == uri') generalRelays

              return $ isInDMRelays || isInGeneralRelays,

            defPropertySigRO' "connectionRetries" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> connectionAttempts rd
                Nothing -> 0,

            defPropertySigRO' "notices" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> notices rd
                Nothing -> []
            ,
            defPropertySigRO' "messagesReceived" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> messagesReceived rd
                Nothing -> 0
            ,
            defPropertySigRO' "eventsReceived" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> eventsReceived rd
                Nothing -> 0
            ,
            defPropertySigRO' "successCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> successCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "failureCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> failureCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "lastConnectedAt" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> maybe 0 id (lastConnectedAt rd)
                Nothing -> 0
            ,
            defPropertySigRO' "lastEoseAt" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> maybe 0 id (lastEoseAt rd)
                Nothing -> 0
            ,
            defPropertySigRO' "authRequiredCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> authRequiredCount rd
                Nothing -> 0
            ,
            defPropertySigRO' "noticeCount" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> noticeCount rd
                Nothing -> 0
            ]

        dmRelayPool <- newFactoryPool (newObject dmRelayClass)
        generalRelayPool <- newFactoryPool (newObject relayClass)
        tempRelayPool <- newFactoryPool (newObject relayClass)

        contextClass <- newClass [
          defPropertySigRO' "dmRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s {
              uiRefs = (uiRefs s) { dmRelaysObjRef = Just obj }
            }
            appState <- runE $ get @AppState
            case keyPair appState of
              Nothing -> return []
              Just kp -> do
                let pk = keyPairToPubKeyXO kp
                relays <- runE $ getDMRelays pk
                mapM (\relay -> getPoolObject dmRelayPool relay) relays,

          defPropertySigRO' "generalRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s {
              uiRefs = (uiRefs s) { generalRelaysObjRef = Just obj }
            }
            appState <- runE $ get @AppState
            case keyPair appState of
                Nothing -> return []
                Just kp -> do
                    let pk = keyPairToPubKeyXO kp
                    relays <- runE $ getGeneralRelays pk
                    mapM (getPoolObject generalRelayPool . getUri) relays,

          defPropertySigRO' "tempRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s {
              uiRefs = (uiRefs s) { tempRelaysObjRef = Just obj }
            }
            poolState <- runE $ get @RelayPoolState
            appState <- runE $ get @AppState

            let activeURIs = Map.keys (activeConnections poolState)

            case keyPair appState of
              Nothing -> return []
              Just kp -> do
                let pk = keyPairToPubKeyXO kp
                dmRelays <- runE $ getDMRelays pk
                generalRelays <- runE $ getGeneralRelays pk
                let generalURIs = map getUri generalRelays

                let tempURIs = filter (\uri -> uri `notElem` dmRelays && uri `notElem` generalURIs) activeURIs
                mapM (getPoolObject tempRelayPool) tempURIs,

          defMethod' "addDMRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            addDMRelay (keyPairToPubKeyXO kp) input,

          defMethod' "removeDMRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            removeDMRelay (keyPairToPubKeyXO kp) input,

          defMethod' "addGeneralRelay" $ \_ input r w -> runE $ do
            kp <- getKeyPair
            addGeneralRelay (keyPairToPubKeyXO kp) input r w,

          defMethod' "removeGeneralRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            removeGeneralRelay (keyPairToPubKeyXO kp) input
          ]

        newObject contextClass ()


-- | Helper function to get connection state text
getConnectionStateText :: RelayURI -> RelayPoolState -> Text
getConnectionStateText uri pst = case Map.lookup uri (activeConnections pst) of
  Just rd -> case connectionState rd of
    Connected -> "Connected"
    Disconnected -> "Disconnected"
    Connecting -> "Connecting"
  Nothing -> "Disconnected"
