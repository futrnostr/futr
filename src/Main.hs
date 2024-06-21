{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Data.Text (pack, unpack)
import Nostr.Keys
    ( derivePubKey,
      keyPairPubKeyXO,
      keyPairSecKey,
      xyToXO,
      createKeyPair,
      secKeyToBech32,
      pubKeyXOToBech32,
      bech32ToSecKey,
      generateMnemonic,
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
    let sk = keyPairSecKey kp
    putStrLn "Secret Key:"
    putStrLn $ unpack $ secKeyToBech32 sk
    putStrLn ""
    putStrLn "Public Key:"
    let (pk, _) = keyPairPubKeyXO kp
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
            putStrLn $ unpack $ secKeyToBech32 sk'
            putStrLn $ show sk'
            let (pk', _) = xyToXO $ derivePubKey sk'
            putStrLn ""
            putStrLn "Public Key:"
            putStrLn $ unpack $ pubKeyXOToBech32 pk'
            putStrLn $ show $ pk'
            putStrLn ""
        Nothing -> error "error"


    -- MNEMONIC TEST

    putStrLn "Mnemonic generating...\n"

    m <- generateMnemonic

    case m of
        Right m' -> do
            putStrLn $ show m'
            putStrLn ""
            mkp <- mnemonicToKeyPair m' ""
            case mkp of
                Right mkp' -> do
                    let secKey = keyPairSecKey mkp'
                    putStrLn "Secret Key:"
                    putStrLn $ unpack $ secKeyToBech32 secKey
                    putStrLn $ show secKey
                    let (mpk', _) = xyToXO $ derivePubKey secKey
                    putStrLn ""
                    putStrLn "Public Key:"
                    putStrLn $ unpack $ pubKeyXOToBech32 mpk'
                    putStrLn $ show $ mpk'
                Left err ->
                    putStrLn $ "Error: " ++ err

        Left err ->
            putStrLn $ "Error: " ++ err

    putStrLn "\nImporting existing mnemonic\n"

    let mnemonic = pack "leader monkey parrot ring guide accident before fence cannon height naive bean"

    putStrLn $ unpack mnemonic ++ "\n"

    haha <- mnemonicToKeyPair mnemonic ""
    case haha of
        Right haha' -> do
            let secKeyH = keyPairSecKey haha'
            putStrLn "Secret Key:"
            putStrLn $ unpack $ secKeyToBech32 secKeyH
            putStrLn $ show secKeyH

            let (haha'', _) = xyToXO $ derivePubKey secKeyH
            putStrLn ""
            putStrLn "Public Key:"
            putStrLn $ unpack $ pubKeyXOToBech32 haha''
            putStrLn $ show $ haha''
        Left err ->
            putStrLn $ "Error: " ++ err

    putStrLn "\nEnd."
