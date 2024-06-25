{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Data.Text (pack, unpack)
import Nostr.Keys
    ( derivePublicKeyXO,
      keyPairToPubKeyXO,
      keyPairToSecKey,
      createKeyPair,
      secretKeyToBech32,
      pubKeyXOToBech32,
      bech32ToSecKey,
      createMnemonic,
      mnemonicToKeyPair )

main :: IO ()
main = do
    putStrLn "-------------------------"
    putStrLn "- nostr testing program -"
    putStrLn "-------------------------"

    -- KEYS TEST

    putStrLn ""
    putStrLn "Generating new key pair\n"
    kp <- createKeyPair
    let sk = keyPairToSecKey kp
    putStrLn "Secret Key:"
    putStrLn $ unpack $ secretKeyToBech32 sk
    putStrLn ""
    putStrLn "Public Key:"
    let pk = keyPairToPubKeyXO kp
    putStrLn $ unpack $ pubKeyXOToBech32 pk
    putStrLn ""
    putStrLn "-------------------------"
    putStrLn ""
    putStrLn "Importing existing key pair\n"
    putStrLn "Imported value:"
    putStrLn "nsec1dshtn5w5uv75cmqgkxdneuj5remp5ye2s3ure24gr43xkc8v0utqxrsgf7\n"
    let nk = bech32ToSecKey "nsec1dshtn5w5uv75cmqgkxdneuj5remp5ye2s3ure24gr43xkc8v0utqxrsgf7"
    case nk of
        Just sk' -> do
            putStrLn "Secret Key:"
            putStrLn $ unpack $ secretKeyToBech32 sk'
            putStrLn $ show sk'
            let pk' = derivePublicKeyXO sk'
            putStrLn ""
            putStrLn "Public Key:"
            putStrLn $ unpack $ pubKeyXOToBech32 pk'
            putStrLn $ show $ pk'
            putStrLn ""
        Nothing -> error "error"


    -- MNEMONIC TEST

    putStrLn "Mnemonic generating...\n"

    m <- createMnemonic

    case m of
        Right m' -> do
            putStrLn $ show m'
            putStrLn ""
            mkp <- mnemonicToKeyPair m' ""
            case mkp of
                Right mkp' -> do
                    let secKey = keyPairToSecKey mkp'
                    putStrLn "Secret Key:"
                    putStrLn $ unpack $ secretKeyToBech32 secKey
                    putStrLn $ show secKey
                    let mpk' = derivePublicKeyXO secKey
                    putStrLn ""
                    putStrLn "Public Key:"
                    putStrLn $ unpack $ pubKeyXOToBech32 mpk'
                    putStrLn $ show $ mpk'
                Left err ->
                    putStrLn $ "Error: " ++ err

        Left err ->
            putStrLn $ "Error: " ++ err

    putStrLn "\nImporting existing 12 word mnemonic\n"

    let mnemonic = pack "leader monkey parrot ring guide accident before fence cannon height naive bean"

    putStrLn $ unpack mnemonic ++ "\n"

    haha <- mnemonicToKeyPair mnemonic ""
    case haha of
        Right haha' -> do
            let secKeyH = keyPairToSecKey haha'
            putStrLn "Secret Key:"
            putStrLn $ unpack $ secretKeyToBech32 secKeyH
            putStrLn $ show secKeyH

            let haha'' = derivePublicKeyXO secKeyH
            putStrLn ""
            putStrLn "Public Key:"
            putStrLn $ unpack $ pubKeyXOToBech32 haha''
            putStrLn $ show $ haha''
        Left err ->
            putStrLn $ "Error: " ++ err

    hata <- mnemonicToKeyPair mnemonic "With Password"
    case hata of
        Right hata' -> do
            let secKeyH = keyPairToSecKey hata'
            putStrLn "Secret Key:"
            putStrLn $ unpack $ secretKeyToBech32 secKeyH
            putStrLn $ show secKeyH

            let hata'' = derivePublicKeyXO secKeyH
            putStrLn ""
            putStrLn "Public Key:"
            putStrLn $ unpack $ pubKeyXOToBech32 hata''
            putStrLn $ show $ hata''
        Left err ->
            putStrLn $ "Error: " ++ err

    -- only 11 words
    let mnemonic2 = pack "monkey parrot ring guide accident before fence cannon height naive bean"

    putStrLn "\n"

    haba <- mnemonicToKeyPair mnemonic2 ""
    case haba of
        Right _ ->
            putStrLn "Should not happen!"
        Left err ->
            putStrLn $ "Perfect: " ++ err

    putStrLn "\nEnd."
