module Main where

import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import EffectfulQML
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import Futr qualified as Futr
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
        . evalState KeyMgmt.initialState
        . KeyMgmt.runKeyMgmt
        . KeyMgmt.runKeyMgmtContext
        . evalState Futr.initialState
        . Futr.runFutrContext
        
        $ do
            changeKey <- createSignalKey
            ctx <- Futr.createCtx changeKey

            let config = QML.defaultEngineConfig
                    { QML.initialDocument = QML.fileDocument path
                    , QML.contextObject = Just $ QML.anyObjRef ctx
                    , QML.importPaths = [importPath, importPath', importPath'']
                    , QML.iconPath = Just ":/icons/nostr-purple.png"
                    }

            KeyMgmt.loadAccounts
            runEngineLoop config changeKey
