module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (State, evalState)
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import QtQuick
import Futr qualified as Futr
import KeyMgmt (KeyMgmtState(..), initialState, runKeyMgmt)
import Logging (runLoggingStdout)
import Nostr
import Nostr.InboxModel (runInboxModel)
import Nostr.Publisher (runPublisher)
import Nostr.RelayConnection (runRelayConnection)
import Nostr.Subscription (runSubscription)
import Nostr.Util (runUtil)
import Presentation.KeyMgmtUI (runKeyMgmtUI)
import Presentation.RelayMgmtUI (runRelayMgmtUI)
import RelayMgmt (runRelayMgmt)
import UI qualified as UI
import Store.Lmdb (LmdbState, initialLmdbState, runLmdbStore)
import Types (AppState(..), RelayPool(..))
import Types qualified as Types


-- | Main function for the app.
main :: IO ()
main = do
    let path = "qrc:/qml/main.qml"
    let importPath = "qrc:/qml"
    let importPath' = "qrc:/qml/content"
    let importPath'' = "qrc:/qml/imports"

    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"
    setEnv "QT_QUICK_CONTROLS_STYLE" "Material"

    runEff
        . runLoggingStdout
        . runConcurrent
        -- state related
        . withInitialState
        -- app related
        . evalState initialQtQuickState
        . runQtQuick
        . runFileSystem
        . runUtil
        . runLmdbStore
        -- nostr related
        . runNostr
        . runKeyMgmt
        . runRelayConnection
        . runPublisher
        . runRelayMgmt
        . runSubscription
        . runInboxModel
        -- presentation related
        . runKeyMgmtUI
        . runRelayMgmtUI
        -- run futr
        . Futr.runFutr
        . UI.runUI
        $ do
            changeKey <- createSignalKey
            ctx <- UI.createUI changeKey

            let config = QML.defaultEngineConfig
                    { QML.initialDocument = QML.fileDocument path
                    , QML.contextObject = Just $ QML.anyObjRef ctx
                    , QML.importPaths = [importPath, importPath', importPath'']
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
