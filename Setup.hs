import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Process
import System.Exit
import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    --hookedPrograms = [simpleProgram "Setup libsec256k1"],
    preConf = \args flags -> do
        let repoDir = "secp256k1"
        repoExists <- doesDirectoryExist repoDir
        unless repoExists $
            callProcess "git" ["clone", "https://github.com/bitcoin-core/secp256k1.git"]
        
        setCurrentDirectory repoDir
        
        callProcess "./autogen.sh" []
        let prefix = "../vendor/secp256k1"
        callProcess "./configure" 
            [ "--prefix=" ++ prefix
            , "--enable-module-schnorrsig"
            , "--enable-module-extrakeys"
            , "--enable-module-ecdh"
            , "--enable-experimental"
            , "--enable-module-recovery"
            ]
        
        callProcess "make" []
        callProcess "make" ["install"]
        
        setCurrentDirectory ".."

        let pkgConfigPath = "PKG_CONFIG_PATH"
        pkgConfigValue <- lookupEnv pkgConfigPath
        let newPkgConfigValue = case pkgConfigValue of
                                 Nothing -> prefix ++ "/lib/pkgconfig"
                                 Just val -> prefix ++ "/lib/pkgconfig:" ++ val
        setEnv pkgConfigPath newPkgConfigValue

        return (Nothing, [])
    }
}
