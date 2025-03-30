module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (State, evalState)
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import QtQuick
import Futr (runFutr)
import KeyMgmt (KeyMgmtState(..), initialState, runKeyMgmt)
import Logging (runLoggingStdout)
import Nostr
import Nostr.EventHandler (runEventHandler)
import Nostr.InboxModel (runInboxModel)
import Nostr.Publisher (runPublisher)
import Nostr.RelayConnection (runRelayConnection)
import Nostr.Subscription (runSubscription)
import Nostr.SubscriptionHandler (runSubscriptionHandler)
import Nostr.Util (runUtil)
import Presentation.Classes (runClasses)
import Presentation.HomeScreen (createUI,runHomeScreen)
import Presentation.KeyMgmtUI (runKeyMgmtUI)
import Presentation.RelayMgmtUI (runRelayMgmtUI)
import RelayMgmt (runRelayMgmt)
import Store.Lmdb (LmdbState, initialLmdbState, runLmdbStore)
import Types (AppState(..), RelayPool(..))
import Types qualified as Types


-- | Main function for the app.
main :: IO ()
main = do
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"
    setEnv "QT_QUICK_CONTROLS_STYLE" "Material"

    runEff
        . runLoggingStdout
        . runConcurrent
        . withInitialState
        . evalState initialQtQuickState
        . runQtQuick
        . runFileSystem
        . runUtil
        . runLmdbStore
        -- nostr related
        . runNostr
        . runKeyMgmt
        . runRelayConnection
        . runEventHandler
        . runPublisher
        . runRelayMgmt
        . runSubscription
        . runSubscriptionHandler
        . runInboxModel
        -- presentation related
        . runKeyMgmtUI
        . runRelayMgmtUI
        . runClasses
        -- run futr
        . runFutr
        . runHomeScreen
        $ do
            changeKey <- createSignalKey
            ctx <- createUI changeKey

            let config = QML.defaultEngineConfig
                    { QML.initialDocument = QML.fileDocument "qrc:/qml/main.qml"
                    , QML.contextObject = Just $ QML.anyObjRef ctx
                    , QML.importPaths =
                        [ "qrc:/qml"
                        , "qrc:/qml/content"
                        , "qrc:/qml/imports"
                    ]
                    , QML.iconPath = Just ":/icons/nostr-purple.png"
                    }

            runEngineLoop config changeKey ctx


-- | Initialize the state for the app.
withInitialState
    :: Eff ( State RelayPool
           : State KeyMgmtState
           : State AppState
           : State LmdbState
           : es) a
    -> Eff es a
withInitialState
    = evalState initialLmdbState
    . evalState Types.initialState
    . evalState KeyMgmt.initialState
    . evalState Types.initialRelayPool
