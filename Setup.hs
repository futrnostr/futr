import Control.Monad (filterM, forM)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.UserHooks (Args, postBuild, preBuild)
import Distribution.Types.BuildInfo
import Distribution.Types.HookedBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.UnqualComponentName
import System.Process (rawSystem)
import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, setModificationTime)
import System.Exit
import System.FilePath ((</>), makeRelative, takeExtension)

main = defaultMainWithHooks simpleUserHooks { preBuild = myPreBuild }

myPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
myPreBuild _ _ = do
    let resourceDir = "resources"
        qrcFile = "resources.qrc"
        touchFileName = "build.touch"

    -- Recursively get all file paths in the resource directory
    allFiles <- listFilesRecursive resourceDir

    -- Filter out directories (we only want files)
    let allFilePaths = filter (not . null) allFiles

    -- Check if the .qrc file exists
    qrcExists <- doesFileExist qrcFile

    rebuildNeeded <- if qrcExists
                     then do
                         qrcTime <- getModificationTime qrcFile
                         anyM (\f -> getModificationTime f >>= \t -> return (t > qrcTime)) allFilePaths
                     else return True

    if rebuildNeeded
    then do
        putStrLn "Resource files modified, regenerating .qrc file."
        generateQrcFile resourceDir qrcFile allFilePaths
        rcc
    else
        putStrLn "No resource file changes detected."

    currentTime <- getPOSIXTime
    setModificationTime touchFileName (posixSecondsToUTCTime currentTime)

    let buildInfo = emptyBuildInfo { cxxSources = ["resources.cpp"] }
    return (Nothing, [(mkUnqualComponentName "futr", buildInfo)])

generateQrcFile :: FilePath -> FilePath -> [FilePath] -> IO ()
generateQrcFile dir qrc files = do
    let qrcContent = "<?xml version=\"1.0\"?>\n<RCC>\n  <qresource prefix=\"/\">\n" ++
                     concatMap (\f -> "    <file alias=\"" ++ makeRelative dir f ++ "\">resources/" ++ makeRelative dir f ++ "</file>\n") files ++
                     "  </qresource>\n</RCC>"
    writeFile qrc qrcContent

rcc :: IO ()
rcc = do
    let qrcFile = "resources.qrc"
        cppFile = "resources.cpp"

    putStrLn $ "Compiling " ++ qrcFile ++ " to " ++ cppFile
    exitCode1 <- rawSystem "rcc" ["-name", "resources", "-o", cppFile, qrcFile]
    if exitCode1 /= ExitSuccess
        then error "Failed to compile .qrc file"
        else return ()

-- Recursively list all files in a directory
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
    contents <- listDirectory dir
    paths <- forM contents $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then listFilesRecursive path
            else return [path]
    return (concat paths)

-- Utility function to check if any file in a list satisfies a condition
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = fmap or . mapM p
