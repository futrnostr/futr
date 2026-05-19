{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip05Search.NamecoinTest (tests) where

import Data.Aeson (Value, decode)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text.Encoding (decodeUtf8)
import Test.Tasty
import Test.Tasty.HUnit

import Nostr.Nip05Search.Namecoin

tests :: IO TestTree
tests = return $ testGroup "Namecoin NIP-05 (.bit) Tests"
  [ testGroup "Identifier Recognition"
      [ testCase "Recognises .bit-suffixed identifiers"  testRecognisesBit
      , testCase "Rejects non-.bit identifiers"          testRejectsNonBit
      ]
  , testGroup "Identifier Parsing"
      [ testCase "user@something.bit"   testParseUserAtBit
      , testCase "_@something.bit"       testParseUnderscoreAtBit
      , testCase "bare .bit domain"      testParseBareBit
      , testCase "d/ direct namespace"   testParseDirectD
      , testCase "id/ direct namespace"  testParseDirectId
      , testCase "Rejects bare @"        testParseRejectBareAt
      , testCase "Lowercases input"      testParseLowercases
      ]
  , testGroup "ifa-0001 value parsing"
      [ testCase "Simple-string nostr field"               testSimpleString
      , testCase "Names map with exact match"              testNamesExact
      , testCase "Names map falls back to _ for root"      testNamesUnderscoreFallback
      , testCase "Names map ignores non-root miss"         testNamesNonRootMiss
      , testCase "Single-identity object (root only)"      testSingleIdentity
      , testCase "Single-identity rejects non-root lookup" testSingleIdentityNonRoot
      , testCase "Relays map keyed by hex pubkey"          testRelaysExtraction
      , testCase "Identity namespace prefers bare pubkey"  testIdentityPubkey
      , testCase "Rejects pubkey of wrong length"          testRejectsBadPubkey
      , testCase "Returns Nothing when no nostr field"     testNoNostrField
      ]
  , testGroup "Electrum scripthash wire format"
      [ testCase "Name index script for d/mstrofnone"  testScriptDmstrofnone
      , testCase "Scripthash for d/mstrofnone"          testScripthashMstrofnone
      , testCase "Scripthash for d/example"             testScripthashExample
      ]
  , testGroup "Default server list"
      [ testCase "Has all six canonical servers" testServerList
      ]
  ]

-- ─── Identifier recognition ─────────────────────────────────────────────

testRecognisesBit :: Assertion
testRecognisesBit = do
  assertBool "alice@example.bit"  $ isNamecoinIdentifier "alice@example.bit"
  assertBool "_@mstrofnone.bit"   $ isNamecoinIdentifier "_@mstrofnone.bit"
  assertBool "bare mstrofnone.bit" $ isNamecoinIdentifier "mstrofnone.bit"
  assertBool "d/mstrofnone"       $ isNamecoinIdentifier "d/mstrofnone"
  assertBool "id/mstrofnone"      $ isNamecoinIdentifier "id/mstrofnone"
  assertBool "uppercase .BIT"     $ isNamecoinIdentifier "ALICE@EXAMPLE.BIT"

testRejectsNonBit :: Assertion
testRejectsNonBit = do
  assertBool "alice@example.com is not Namecoin" $
    not (isNamecoinIdentifier "alice@example.com")
  assertBool "npub1abc is not Namecoin" $
    not (isNamecoinIdentifier "npub1abc")
  assertBool "empty is not Namecoin" $
    not (isNamecoinIdentifier "")

-- ─── Identifier parsing ─────────────────────────────────────────────────

testParseUserAtBit :: Assertion
testParseUserAtBit =
  parseNamecoinIdentifier "alice@example.bit"
    @?= Just (ParsedNamecoinId "d/example" "alice" Domain)

testParseUnderscoreAtBit :: Assertion
testParseUnderscoreAtBit =
  parseNamecoinIdentifier "_@mstrofnone.bit"
    @?= Just (ParsedNamecoinId "d/mstrofnone" "_" Domain)

testParseBareBit :: Assertion
testParseBareBit =
  parseNamecoinIdentifier "mstrofnone.bit"
    @?= Just (ParsedNamecoinId "d/mstrofnone" "_" Domain)

testParseDirectD :: Assertion
testParseDirectD =
  parseNamecoinIdentifier "d/mstrofnone"
    @?= Just (ParsedNamecoinId "d/mstrofnone" "_" Domain)

testParseDirectId :: Assertion
testParseDirectId =
  parseNamecoinIdentifier "id/mstrofnone"
    @?= Just (ParsedNamecoinId "id/mstrofnone" "_" Identity)

testParseRejectBareAt :: Assertion
testParseRejectBareAt = do
  parseNamecoinIdentifier "@.bit" @?= Nothing
  parseNamecoinIdentifier "alice@example.com" @?= Nothing
  parseNamecoinIdentifier "" @?= Nothing

testParseLowercases :: Assertion
testParseLowercases =
  parseNamecoinIdentifier "Alice@Example.BIT"
    @?= Just (ParsedNamecoinId "d/example" "alice" Domain)

-- ─── ifa-0001 value parsing ─────────────────────────────────────────────

-- Hex public keys (32 bytes / 64 hex chars).
pkA, pkB :: BSL.ByteString
pkA = "43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0"
pkB = "1111111111111111111111111111111111111111111111111111111111111111"

testSimpleString :: Assertion
testSimpleString =
  let v = mustDecode "{\"nostr\":\"43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0\"}"
  in extractNostrFromValue Domain "_" v
       @?= Just ("43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0", [])

testNamesExact :: Assertion
testNamesExact =
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"names\":{\"alice\":\""
        , pkA
        , "\",\"bob\":\""
        , pkB
        , "\"}}}"
        ]
  in extractNostrFromValue Domain "alice" v
       @?= Just ("43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0", [])

testNamesUnderscoreFallback :: Assertion
testNamesUnderscoreFallback =
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"names\":{\"_\":\""
        , pkA
        , "\"}}}"
        ]
  in extractNostrFromValue Domain "_" v
       @?= Just ("43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0", [])

testNamesNonRootMiss :: Assertion
testNamesNonRootMiss =
  -- A names-only record with no "alice" key and no "_" must not hand
  -- alice@example.bit the bob identity from a "bob" entry. This is the
  -- precedence rule the Kotlin reference enforces in extractFromDomainValue.
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"names\":{\"bob\":\""
        , pkB
        , "\"}}}"
        ]
  in extractNostrFromValue Domain "alice" v @?= Nothing

testSingleIdentity :: Assertion
testSingleIdentity =
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"pubkey\":\""
        , pkA
        , "\",\"relays\":[\"wss://relay.example/\",\"wss://r2.example/\"]}}"
        ]
  in extractNostrFromValue Domain "_" v
       @?= Just ( "43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0"
                , ["wss://relay.example/", "wss://r2.example/"]
                )

testSingleIdentityNonRoot :: Assertion
testSingleIdentityNonRoot =
  -- alice@example.bit must not be answered by example.bit's single-
  -- identity pubkey field — that would leak the root operator's
  -- identity to a different local-part.
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"pubkey\":\""
        , pkA
        , "\"}}"
        ]
  in extractNostrFromValue Domain "alice" v @?= Nothing

testRelaysExtraction :: Assertion
testRelaysExtraction =
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"names\":{\"_\":\""
        , pkA
        , "\"},\"relays\":{\""
        , pkA
        , "\":[\"wss://relay.testls.bit/\"]}}}"
        ]
  in extractNostrFromValue Domain "_" v
       @?= Just ( "43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0"
                , ["wss://relay.testls.bit/"]
                )

testIdentityPubkey :: Assertion
testIdentityPubkey =
  -- id/ records are looked up under the Identity namespace where the
  -- "pubkey" field is the natural place to find the user's key.
  let v = mustDecode $ BSL.concat
        [ "{\"nostr\":{\"pubkey\":\""
        , pkA
        , "\"}}"
        ]
  in extractNostrFromValue Identity "_" v
       @?= Just ("43185edecb675892824b1a37a57f3e407fbde2eda7201a3829b8cf4ba7c5b4f0", [])

testRejectsBadPubkey :: Assertion
testRejectsBadPubkey = do
  -- 63 hex chars (off by one)
  let v1 = mustDecode "{\"nostr\":\"431111111111111111111111111111111111111111111111111111111111111\"}"
  extractNostrFromValue Domain "_" v1 @?= Nothing
  -- Not hex
  let v2 = mustDecode "{\"nostr\":\"zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\"}"
  extractNostrFromValue Domain "_" v2 @?= Nothing

testNoNostrField :: Assertion
testNoNostrField = do
  let v1 = mustDecode "{\"email\":\"alice@example.com\"}"
  extractNostrFromValue Domain "_" v1 @?= Nothing
  let v2 = mustDecode "{\"nostr\":12345}"
  extractNostrFromValue Domain "_" v2 @?= Nothing

-- ─── Scripthash wire format ─────────────────────────────────────────────

testScriptDmstrofnone :: Assertion
testScriptDmstrofnone =
  -- OP_3 0c d/mstrofnone 00 6d 75 6a
  let script = buildNameIndexScript "d/mstrofnone"
  in decodeUtf8 (B16.encode script)
       @?= "530c642f6d7374726f666e6f6e65006d756a"

testScripthashMstrofnone :: Assertion
testScripthashMstrofnone =
  -- Cross-checked against the reference Python computation in the PR
  -- description; matches the scripthash the Kotlin / Dart / TS clients
  -- send for the same name.
  electrumScriptHash (buildNameIndexScript "d/mstrofnone")
    @?= "4975bc3120749faf95ae70a06bf3d83a697cbb56a9bc17f1ffa252e456137557"

testScripthashExample :: Assertion
testScripthashExample =
  electrumScriptHash (buildNameIndexScript "d/example")
    @?= "92f51fe51fa9c53ea842325c9c63a9b8592b3f64c7477b6e538a64d80cd8c0ac"

-- ─── Server list ────────────────────────────────────────────────────────

testServerList :: Assertion
testServerList = do
  -- Mirrors DEFAULT_ELECTRUMX_SERVERS in
  -- quartz/.../namecoin/ElectrumXServer.kt at the time of writing.
  -- Both servers and ports are compared so a wire-format drift in
  -- either field surfaces as a CI failure.
  defaultElectrumxServers @?=
    [ ElectrumxServer "electrumx.testls.space"      50002
    , ElectrumxServer "nmc2.bitcoins.sk"            57002
    , ElectrumxServer "46.229.238.187"              57002
    , ElectrumxServer "relay.testls.bit"            50002
    , ElectrumxServer "23.158.233.10"               50002
    , ElectrumxServer "electrum.nmc.ethicnology.com" 50002
    ]

-- ─── Helpers ────────────────────────────────────────────────────────────

mustDecode :: BSL.ByteString -> Value
mustDecode bs = case decode bs of
  Just v  -> v
  Nothing -> error ("test fixture has malformed JSON: " <> show bs)
