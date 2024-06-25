-- | Module: Nostr.Kind
-- This module defines the 'Kind' data type and instances for JSON parsing and serialization.

module Nostr.Kind where

import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withScientific, Value(Number))
import Control.Monad (mzero)

-- | The 'Kind' data type represents different kinds of events in the Nostr protocol.
data Kind
  = Metadata        -- NIP-01
  | ShortTextNote   -- NIP-01
  | Follows         -- NIP-01
  | EventDeletion   -- NIP-09
  | Repost          -- NIP-18
  | Reaction        -- NIP-25
  | Seal            -- NIP-59
  | DirectMessage   -- NIP-17
  deriving (Eq, Show)

-- | 'FromJSON' instance for 'Kind'.
-- This allows parsing JSON numbers into 'Kind' values.
instance FromJSON Kind where
  parseJSON = withScientific "kind" $ \k -> case k of
    0  -> return Metadata
    1  -> return ShortTextNote
    3  -> return Follows
    5  -> return EventDeletion
    6  -> return Repost
    7  -> return Reaction
    13 -> return Seal
    14 -> return DirectMessage
    _  -> mzero

-- | 'ToJSON' instance for 'Kind'.
-- This allows serializing 'Kind' values into JSON numbers.
instance ToJSON Kind where
  toJSON Metadata      = Number 0
  toJSON ShortTextNote = Number 1
  toJSON Follows       = Number 3
  toJSON EventDeletion = Number 5
  toJSON Repost        = Number 6
  toJSON Reaction      = Number 7
  toJSON Seal          = Number 13
  toJSON DirectMessage = Number 14

-- kind - description - nip
-- 0 	User Metadata 	01
-- 1 	Short Text Note 	01
-- 2 	Recommend Relay 	01 (deprecated)
-- 3 	Follows 	02
-- 4 	Encrypted Direct Messages 	04
-- 5 	Event Deletion 	09
-- 6 	Repost 	18
-- 7 	Reaction 	25
-- 8 	Badge Award 	58
-- 9 	Group Chat Message 	29
-- 10 	Group Chat Threaded Reply 	29
-- 11 	Group Thread 	29
-- 12 	Group Thread Reply 	29
-- 13 	Seal 	59
-- 14 	Direct Message 	17
-- 16 	Generic Repost 	18
-- 40 	Channel Creation 	28
-- 41 	Channel Metadata 	28
-- 42 	Channel Message 	28
-- 43 	Channel Hide Message 	28
-- 44 	Channel Mute User 	28
-- 818 	Merge Requests 	54
-- 1021 	Bid 	15
-- 1022 	Bid confirmation 	15
-- 1040 	OpenTimestamps 	03
-- 1059 	Gift Wrap 	59
-- 1063 	File Metadata 	94
-- 1311 	Live Chat Message 	53
-- 1617 	Patches 	34
-- 1621 	Issues 	34
-- 1622 	Replies 	34
-- 1630-1633 	Status 	34
-- 1971 	Problem Tracker 	nostrocket
-- 1984 	Reporting 	56
-- 1985 	Label 	32
-- 2003 	Torrent 	35
-- 2004 	Torrent Comment 	35
-- 2022 	Coinjoin Pool 	joinstr
-- 4550 	Community Post Approval 	72
-- 5000-5999 	Job Request 	90
-- 6000-6999 	Job Result 	90
-- 7000 	Job Feedback 	90
-- 9000-9030 	Group Control Events 	29
-- 9041 	Zap Goal 	75
-- 9734 	Zap Request 	57
-- 9735 	Zap 	57
-- 9802 	Highlights 	84
-- 10000 	Mute list 	51
-- 10001 	Pin list 	51
-- 10002 	Relay List Metadata 	65
-- 10003 	Bookmark list 	51
-- 10004 	Communities list 	51
-- 10005 	Public chats list 	51
-- 10006 	Blocked relays list 	51
-- 10007 	Search relays list 	51
-- 10009 	User groups 	51, 29
-- 10015 	Interests list 	51
-- 10030 	User emoji list 	51
-- 10050 	Relay list to receive DMs 	17
-- 10096 	File storage server list 	96
-- 13194 	Wallet Info 	47
-- 21000 	Lightning Pub RPC 	Lightning.Pub
-- 22242 	Client Authentication 	42
-- 23194 	Wallet Request 	47
-- 23195 	Wallet Response 	47
-- 24133 	Nostr Connect 	46
-- 27235 	HTTP Auth 	98
-- 30000 	Follow sets 	51
-- 30001 	Generic lists 	51
-- 30002 	Relay sets 	51
-- 30003 	Bookmark sets 	51
-- 30004 	Curation sets 	51
-- 30005 	Video sets 	51
-- 30008 	Profile Badges 	58
-- 30009 	Badge Definition 	58
-- 30015 	Interest sets 	51
-- 30017 	Create or update a stall 	15
-- 30018 	Create or update a product 	15
-- 30019 	Marketplace UI/UX 	15
-- 30020 	Product sold as an auction 	15
-- 30023 	Long-form Content 	23
-- 30024 	Draft Long-form Content 	23
-- 30030 	Emoji sets 	51
-- 30063 	Release artifact sets 	51
-- 30078 	Application-specific Data 	78
-- 30311 	Live Event 	53
-- 30315 	User Statuses 	38
-- 30402 	Classified Listing 	99
-- 30403 	Draft Classified Listing 	99
-- 30617 	Repository announcements 	34
-- 30818 	Wiki article 	54
-- 30819 	Redirects 	54
-- 31890 	Feed 	NUD: Custom Feeds
-- 31922 	Date-Based Calendar Event 	52
-- 31923 	Time-Based Calendar Event 	52
-- 31924 	Calendar 	52
-- 31925 	Calendar Event RSVP 	52
-- 31989 	Handler recommendation 	89
-- 31990 	Handler information 	89
-- 34235 	Video Event 	71
-- 34236 	Short-form Portrait Video Event 	71
-- 34237 	Video View Event 	71
-- 34550 	Community Definition 	72
-- 39000-9 	Group metadata events 	29
