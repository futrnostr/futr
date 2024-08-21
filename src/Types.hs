{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Types where

data AppScreen
    = Welcome
    | KeyMgmt
    | Relay
    | Home
    deriving (Eq, Read, Show)
