module Helpers where

import AppTypes   (Keys)
import NostrTypes (Relay(..))

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

mainKey :: [Keys] -> Keys
mainKey ks = head $ filter (\k -> third k == True) ks

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\k -> (fst' k, snd' k, False)) ks

poolWithoutRelay :: [Relay] -> Relay -> [Relay]
poolWithoutRelay p r = p' where
    p' = filter (\r' -> not $ r `sameRelay` r') p

sameRelay :: Relay -> Relay -> Bool
sameRelay a b = host a == host b && port a == port b

sortPool :: Relay -> Relay -> Ordering
sortPool a b = mconcat [compare (host a) (host b), compare (port a) (port b) ]
