module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import EffectfulQML
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import AppState qualified as AppState
import Futr qualified as Futr
import Nostr.Effects.CurrentTime (runCurrentTime)
import Nostr.Effects.IDGen (runIDGen)
import Nostr.Effects.Logging (runLoggingStdout)
import Nostr.Effects.RelayPool (runRelayPool)
import Nostr.Effects.WebSocket (runWebSocket)
import Presentation.KeyMgmt qualified as KeyMgmt
import UI qualified as UI

main :: IO ()
main = do
    let path = "qrc:/qml/main.qml"
    let importPath = "qrc:/qml"
    let importPath' = "qrc:/qml/content"
    let importPath'' = "qrc:/qml/imports"
    let qtQuickControls = "resources/qml/qtquickcontrols2.conf" -- @todo move out of the way

    setEnv "QT_QUICK_CONTROLS_CONF" qtQuickControls
    setEnv "QT_AUTO_SCREEN_SCALE_FACTOR" "1"
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"
    setEnv "QT_ENABLE_HIGHDPI_SCALING" "1"

    runEff
        . runEffectfulQML
        . runFileSystem
        . runIDGen
        . runCurrentTime
        . runLoggingStdout
        . runConcurrent
        . evalState KeyMgmt.initialState
        . evalState AppState.initialRelayPoolState
        . KeyMgmt.runKeyMgmt
        . KeyMgmt.runKeyMgmtUI
        . evalState AppState.initialState
        . runWebSocket
        . runRelayPool
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

            runEngineLoop config changeKey
