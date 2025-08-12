{-# LANGUAGE BlockArguments #-}

module Nostr.FilterSet
  ( inboxMentionsFilter
  , profilesFilterFS
  , userPostsFilterFS
  , commentsFilterFS
  , filtersStructurallyChanged
  ) where

import Prelude hiding (until)
import Data.Map.Strict qualified as Map
import Nostr.Keys (PubKeyXO, byteStringToHex, exportPubKeyXO)
import Nostr.Event (EventId(..), Kind(..), eventIdToHex)
import Nostr.Types (Filter(..), emptyFilter)

-- | Mentions and interactions targeting a user. Optional since.
--   Spam-safety not wired here; call sites can refine authors if desired.
inboxMentionsFilter :: PubKeyXO -> Maybe Int -> Filter
inboxMentionsFilter pk ts = emptyFilter
  { kinds = Just [ShortTextNote, Repost, Comment, EventDeletion]
  , fTags = Just $ Map.singleton 'p' [byteStringToHex $ exportPubKeyXO pk]
  , since = ts
  }

-- | Profiles / metadata related filters for a set of authors
profilesFilterFS :: [PubKeyXO] -> Filter
profilesFilterFS pks = emptyFilter
  { authors = Just pks
  , kinds = Just [RelayListMetadata, PreferredDMRelays, FollowList, Metadata]
  }

-- | Public posts and interactions by authors, optional since/until/limit (limit default scaled)
userPostsFilterFS :: [PubKeyXO] -> Maybe Int -> Maybe Int -> Filter
userPostsFilterFS pks s ml = emptyFilter
  { authors = Just pks
  , kinds = Just [ShortTextNote, Repost, EventDeletion]
  , since = s
  , limit = case ml of
      Just l -> Just l
      Nothing -> Just $ 500 * length pks
  }

-- | Comments filter for a specific root event
commentsFilterFS :: EventId -> Filter
commentsFilterFS eid = emptyFilter
  { kinds = Just [ShortTextNote]
  , fTags = Just $ Map.singleton 'e' [eventIdToHex eid]
  , limit = Just 2000
  }


-- | Structural filter change detection used for reconciliation/resubscribe decisions.
-- Ignores since-only forward moves; resends on structural/backwards changes.
filtersStructurallyChanged :: Filter -> Filter -> Bool
filtersStructurallyChanged oldF newF =
  (authors oldF /= authors newF) ||
  (ids oldF /= ids newF) ||
  (fTags oldF /= fTags newF) ||
  (kinds oldF /= kinds newF) ||
  (until oldF /= until newF) ||
  case (since oldF, since newF) of
    (Nothing, Just _) -> True
    (Just o, Just n) | n < o -> True
    _ -> False

