module Main where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Graphics.QML qualified as QML
import System.Environment (setEnv)

import Downloader (initialDownloaderState, runDownloader)
import Futr (runFutr)
import KeyMgmt (initialKeyMgmtState, runKeyMgmt)
import Logging (runLoggingStdout)
import Nostr (runNostr)
import Nostr.InboxModel (runInboxModel)
import Nostr.ProfileManager (initialProfileManagerState, runProfileManager)
import Nostr.Publisher (runPublisher)
import Nostr.Relay (initialRelayPool, runRelayConnection)
import Nostr.Util (runUtil)
import Presentation.Classes (runClasses)
import Presentation.HomeScreen (createUI,runHomeScreen)
import Presentation.KeyMgmtUI (runKeyMgmtUI)
import Presentation.RelayMgmtUI (runRelayMgmtUI)
import QtQuick
import RelayMgmt (runRelayMgmt)
import Store.Lmdb (initialLmdbState, runLmdbStore)
import Types (initialState)


-- | Main function for the app.
main :: IO ()
main = do
    -- Only works on linux, for debugging only
    setEnv "QT_LOGGING_RULES" "qt.qml.connections=false"

    runEff
        . runLoggingStdout
        . runConcurrent
        . evalState initialRelayPool
        . evalState initialProfileManagerState
        . evalState initialLmdbState
        . evalState initialState
        . evalState initialKeyMgmtState
        . evalState initialQtQuickState
        . evalState initialDownloaderState
        . runQtQuick
        . runFileSystem
        . runUtil
        . runLmdbStore
        . runDownloader
        -- nostr related
        . runNostr
        . runKeyMgmt
        . runRelayConnection
        . runPublisher
        . runRelayMgmt
        . runInboxModel
        . runProfileManager
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

            --liftIO $ QML.setQtFlag QML.QtEnableQMLDebug True
            liftIO $ QML.enableHighDpiScaling
            runEngineLoop config changeKey ctx
