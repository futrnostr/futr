import Control.Monad (filterM, forM)
import Data.List (intercalate)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Distribution.Package (pkgVersion)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.UserHooks (Args, postBuild, preBuild)
import Distribution.Types.BuildInfo
import Distribution.Types.HookedBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(..), packageDescription)
import Distribution.Types.UnqualComponentName
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Fields.ParseResult (ParseResult(..), runParseResult)
import Distribution.Version (versionNumbers)
import System.Process (rawSystem)
import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, setModificationTime)
import System.Exit
import System.FilePath ((</>), makeRelative, takeExtension)
import System.Info (os)
import qualified Data.ByteString.Char8 as BS

main = defaultMainWithHooks simpleUserHooks { preBuild = myPreBuild }

myPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
myPreBuild _ _ = do
    -- Generate version-dependent files
    generateVersionFiles

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

    -- Conditionally compile Windows resources
    windowsResources <- if os == "mingw32"
                       then compileWindowsResources
                       else return []

    currentTime <- getPOSIXTime
    setModificationTime touchFileName (posixSecondsToUTCTime currentTime)

    let buildInfo = emptyBuildInfo {
        cxxSources = ["resources.cpp"],
        ldOptions = windowsResources
    }
    return (Nothing, [(mkUnqualComponentName "futr", buildInfo)])

-- | Generate all version-dependent files from templates
generateVersionFiles :: IO ()
generateVersionFiles = do
    putStrLn "Generating version-dependent files..."

    -- Read version from cabal file
    (versionDot, versionNumbers) <- readVersionFromCabal
    let versionComma = intercalate "," (map show versionNumbers)

    -- Generate the Version.hs module
    generateVersionModule versionDot

    -- Generate files from templates
    generateFromTemplate "platform/windows/futr.rc.template" "platform/windows/futr.rc"
        [("@VERSION@", versionDot), ("@VERSION_STRING@", versionDot), ("@VERSION_COMMA@", versionComma)]

    generateFromTemplate "platform/windows/copy-dlls.sh.template" "platform/windows/copy-dlls.sh"
        [("@VERSION@", versionDot)]

    generateFromTemplate "platform/windows/innosetup.iss.template" "platform/windows/innosetup.iss"
        [("@VERSION@", versionDot)]

    generateFromTemplate "flatpak/com.futrnostr.futr.yml.template" "flatpak/com.futrnostr.futr.yml"
        [("@VERSION@", versionDot)]

    putStrLn $ "Generated files with version: " ++ versionDot

-- | Generate a simple Version.hs module with version constants
generateVersionModule :: String -> IO ()
generateVersionModule version = do
    let versionContent = unlines
            [ "-- | Auto-generated version module. Do not edit directly!"
            , "-- | Edit version in futr.cabal instead."
            , "module Version where"
            , ""
            , "-- | Version string extracted from cabal file"
            , "versionString :: String"
            , "versionString = " ++ show version
            , ""
            , "-- | Runtime version in 'v0.4.0' format"
            , "runtimeVersion :: String"
            , "runtimeVersion = \"v\" ++ versionString"
            ]
    writeFile "src/Version.hs" versionContent
    putStrLn "Generated: src/Version.hs"

-- | Read version from cabal file
readVersionFromCabal :: IO (String, [Int])
readVersionFromCabal = do
    contents <- BS.readFile "futr.cabal"
    case runParseResult (parseGenericPackageDescription contents) of
        (_, Right pkg) -> do
            let version = pkgVersion $ package $ packageDescription pkg
                versionList = versionNumbers version
                versionString = intercalate "." $ map show versionList
            return (versionString, versionList)
        (_, Left err) -> do
            error $ "Failed to parse cabal file: " ++ show err

-- | Generate a file from a template by replacing placeholders
generateFromTemplate :: FilePath -> FilePath -> [(String, String)] -> IO ()
generateFromTemplate templatePath outputPath replacements = do
    templateExists <- doesFileExist templatePath
    if templateExists
    then do
        template <- readFile templatePath
        let result = foldl (\content (placeholder, replacement) ->
                         replaceAll placeholder replacement content) template replacements
        writeFile outputPath result
        putStrLn $ "Generated: " ++ outputPath
    else
        putStrLn $ "Warning: Template not found: " ++ templatePath

-- | Replace all occurrences of a substring
replaceAll :: String -> String -> String -> String
replaceAll [] _ haystack = haystack
replaceAll _ _ [] = []
replaceAll needle replacement haystack
    | take (length needle) haystack == needle =
        replacement ++ replaceAll needle replacement (drop (length needle) haystack)
    | otherwise =
        head haystack : replaceAll needle replacement (tail haystack)

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

-- Compile Windows resource file to object file
compileWindowsResources :: IO [String]
compileWindowsResources = do
    let rcFile = "platform/windows/futr.rc"
        objFile = "platform/windows/futr_res.o"

    rcExists <- doesFileExist rcFile
    if not rcExists
    then do
        putStrLn "Warning: Windows resource file not found, skipping resource compilation"
        return []
    else do
        putStrLn $ "Compiling Windows resources: " ++ rcFile ++ " to " ++ objFile
        exitCode <- rawSystem "windres" [rcFile, "-o", objFile]
        if exitCode /= ExitSuccess
        then do
            putStrLn "Warning: Failed to compile Windows resources (windres not found or failed)"
            return []
        else do
            putStrLn "Windows resources compiled successfully"
            return [objFile]

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
