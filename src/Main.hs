module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import EffectfulQML
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import Futr qualified as Futr
import Nostr.Effects.Dispatcher (runDispatcher)
import Nostr.Effects.IDGen (runIDGen)
import Nostr.Effects.Logging (runLoggingStdout)
import Nostr.Effects.RelayPool (runRelayPool)
import Nostr.Effects.WebSocket (runWebSocket)
import Presentation.KeyMgmt qualified as KeyMgmt

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
        . runLoggingStdout
        . runConcurrent
        . evalState KeyMgmt.initialState
        . KeyMgmt.runKeyMgmt
        . KeyMgmt.runKeyMgmtUI
        . evalState Futr.initialState
        . runDispatcher
        . runWebSocket
        . runRelayPool
        . Futr.runFutr
        . Futr.runFutrUI
        $ do
            changeKey <- createSignalKey
            ctx <- Futr.createUI changeKey

            let config = QML.defaultEngineConfig
                    { QML.initialDocument = QML.fileDocument path
                    , QML.contextObject = Just $ QML.anyObjRef ctx
                    , QML.importPaths = [importPath, importPath', importPath'']
                    , QML.iconPath = Just ":/icons/nostr-purple.png"
                    }

            runEngineLoop config changeKey
