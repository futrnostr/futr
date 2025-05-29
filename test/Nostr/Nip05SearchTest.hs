{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip05SearchTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Nostr.Nip05Search

tests :: IO TestTree
tests = return $ testGroup "NIP-05 Search Tests"
  [ testGroup "Identifier Recognition"
    [ testCase "Valid NIP-05 identifiers" testValidIdentifiers
    , testCase "Invalid NIP-05 identifiers" testInvalidIdentifiers
    ]
  , testGroup "Identifier Parsing"
    [ testCase "Valid parsing cases" testValidParsing
    , testCase "Invalid parsing cases" testInvalidParsing
    ]
  ]


testValidIdentifiers :: Assertion
testValidIdentifiers = do
  assertBool "jack@cash.app should be valid" $ isNip05Identifier "jack@cash.app"
  assertBool "test@example.com should be valid" $ isNip05Identifier "test@example.com"
  assertBool "user@domain.org should be valid" $ isNip05Identifier "user@domain.org"
  assertBool "_@example.com should be valid" $ isNip05Identifier "_@example.com"
  assertBool "jack@x.com should be valid" $ isNip05Identifier "jack@x.com"


testInvalidIdentifiers :: Assertion
testInvalidIdentifiers = do
  assertBool "npub1abc123 should not be valid" $ not $ isNip05Identifier "npub1abc123"
  assertBool "nprofile1xyz should not be valid" $ not $ isNip05Identifier "nprofile1xyz"
  assertBool "note1abc should not be valid" $ not $ isNip05Identifier "note1abc"
  assertBool "nevent1xyz should not be valid" $ not $ isNip05Identifier "nevent1xyz"
  assertBool "naddr1abc should not be valid" $ not $ isNip05Identifier "naddr1abc"
  assertBool "invalid should not be valid" $ not $ isNip05Identifier "invalid"
  assertBool "@domain.com should not be valid" $ not $ isNip05Identifier "@domain.com"
  assertBool "user@ should not be valid" $ not $ isNip05Identifier "user@"
  assertBool "empty string should not be valid" $ not $ isNip05Identifier ""
  assertBool "double @ should not be valid" $ not $ isNip05Identifier "user@@domain.com"
  assertBool "no @ should not be valid" $ not $ isNip05Identifier "userdomain.com"


testValidParsing :: Assertion
testValidParsing = do
  parseNip05Identifier "jack@cash.app" @?= Just ("jack", "cash.app")
  parseNip05Identifier "test@example.com" @?= Just ("test", "example.com")
  parseNip05Identifier "user@domain.org" @?= Just ("user", "domain.org")
  parseNip05Identifier "_@example.com" @?= Just ("_", "example.com")
  parseNip05Identifier "jack@x.com" @?= Just ("jack", "x.com")

  -- Test with whitespace (should be trimmed)
  parseNip05Identifier " jack@cash.app " @?= Just ("jack", "cash.app")
  parseNip05Identifier "\tjack@cash.app\n" @?= Just ("jack", "cash.app")


testInvalidParsing :: Assertion
testInvalidParsing = do
  parseNip05Identifier "npub1abc123" @?= Nothing
  parseNip05Identifier "nprofile1xyz" @?= Nothing
  parseNip05Identifier "invalid" @?= Nothing
  parseNip05Identifier "@domain.com" @?= Nothing
  parseNip05Identifier "user@" @?= Nothing
  parseNip05Identifier "" @?= Nothing
  parseNip05Identifier "user@@domain.com" @?= Nothing
  parseNip05Identifier "userdomain.com" @?= Nothing
  parseNip05Identifier "user@domain@com" @?= Nothing
