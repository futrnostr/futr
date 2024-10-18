module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import EffectfulQML
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import Futr qualified as Futr
import Logging (runLoggingStdout)
import Nostr.GiftWrap (runGiftWrap)
import Nostr.RelayPool (runRelayPool)
import Nostr.WebSocket (runWebSocket)
import Nostr.Util (runUtil)
import Presentation.KeyMgmt qualified as KeyMgmt
import UI qualified as UI
import Types

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
        . runEffectfulQML
        . runFileSystem
        . runUtil
        . runConcurrent
        . evalState KeyMgmt.initialState
        . evalState Types.initialRelayPoolState
        . KeyMgmt.runKeyMgmt
        . KeyMgmt.runKeyMgmtUI
        . evalState Types.initialState
        . runGiftWrap
        . runWebSocket 3 -- max 3 retries
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

            runEngineLoop config changeKey ctx
