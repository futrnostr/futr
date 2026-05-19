-- | Module: Nostr.Nip05Search.Namecoin
--
-- Namecoin (".bit") variant of NIP-05 verification.
--
-- This module is a Haskell port of the ifa-0001 + ElectrumX wire format
-- used by Amethyst (Kotlin), Nostur (Swift), nostrmo (Dart), and the
-- in-review nostr-tools / nostrudel / jumble JS implementations. It
-- accepts the same identifier shapes those clients accept and produces
-- the same shape of result as 'Nostr.Nip05Search.Nip05SearchResult'
-- so the rest of futr does not have to care about the transport.
--
-- Wire format references:
--
--   * ifa-0001 Domain Name Object (the JSON value Namecoin returns
--     for @d\/@-prefixed names): the @nostr@ key may be either a hex
--     pubkey string, or an object with a @names@ map and a @relays@
--     map keyed by hex pubkey (same shape as a NIP-05 nostr.json), or
--     an object with a bare @pubkey@ field plus optional @relays@
--     array for single-identity records.
--   * ElectrumX 1.4.1: @blockchain.scripthash.get_history@ +
--     @blockchain.transaction.get@ with verbose=true. The scripthash
--     is computed over the canonical Namecoin name index script
--     (@OP_NAME_UPDATE \<push name\> \<push empty\> OP_2DROP OP_DROP OP_RETURN@)
--     and then byte-reversed and hex-encoded.
--   * Both @NAME_FIRSTUPDATE@ (OP_2, 0x52) and @NAME_UPDATE@ (OP_3,
--     0x53) outputs are decoded so first-update records resolve
--     correctly. The FIRSTUPDATE script has an extra @<rand>@ push
--     between name and value which is skipped.
--
-- See N1 NIP draft and
-- @quartz\/src\/commonMain\/kotlin\/com\/vitorpamplona\/quartz\/nip05DnsIdentifiers\/namecoin@
-- in vitorpamplona\/amethyst for the canonical reference impl.

{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip05Search.Namecoin
  ( -- * Public API
    isNamecoinIdentifier
  , searchNamecoinNip05
    -- * Pure ifa-0001 helpers (exported for testing)
  , parseNamecoinIdentifier
  , ParsedNamecoinId(..)
  , Namespace(..)
  , extractNostrFromValue
  , buildNameIndexScript
  , electrumScriptHash
    -- * Server configuration (exported for testing / overrides)
  , ElectrumxServer(..)
  , defaultElectrumxServers
  ) where

import Control.Exception (SomeException, bracket, try)
import Data.Default (def)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
  ( Value(..), decode, eitherDecodeStrict, encode, object, (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Vector qualified as V
import Data.Word (Word8)
import Network.Connection
  ( Connection, ConnectionParams(..)
  , connectTo, connectionClose, connectionGetLine, connectionPut
  , initConnectionContext
  )
import System.Timeout (timeout)

import Nostr.Keys (PubKeyXO, pubKeyXOFromHex)

-- ─────────────────────────────────────────────────────────────────────
-- Server configuration
-- ─────────────────────────────────────────────────────────────────────

-- | A single ElectrumX endpoint.
--
-- All entries in 'defaultElectrumxServers' use TLS over TCP on the
-- standard Electrum @50002@\/@57002@ ports. Per-host SHA-256 cert
-- pinning (the @usePinnedTrustStore@ knob in the Kotlin reference) is
-- intentionally not implemented in this initial port — see the
-- module-level note in the PR body for the N3 / TLSA TOFU follow-up.
-- For now we connect using the system trust store; the
-- @electrum.nmc.ethicnology.com@ entry (Let\'s Encrypt) verifies under
-- the default trust manager, and the rest are best-effort fallbacks.
data ElectrumxServer = ElectrumxServer
  { electrumHost :: Text
  , electrumPort :: Int
  } deriving (Eq, Show)

-- | The canonical six-entry public Namecoin ElectrumX server list,
-- mirroring @DEFAULT_ELECTRUMX_SERVERS@ in Amethyst\'s
-- @ElectrumXServer.kt@ at the time of writing. Servers are tried in
-- order; the first one to return a definitive answer wins.
--
-- This is exported so callers can substitute a different list (for
-- tests, Tor-only setups, or custom deployments) without touching the
-- resolver.
defaultElectrumxServers :: [ElectrumxServer]
defaultElectrumxServers =
  [ ElectrumxServer "electrumx.testls.space"      50002
  , ElectrumxServer "nmc2.bitcoins.sk"            57002
  , ElectrumxServer "46.229.238.187"              57002
  , ElectrumxServer "relay.testls.bit"            50002
  , ElectrumxServer "23.158.233.10"               50002
  , ElectrumxServer "electrum.nmc.ethicnology.com" 50002
  ]

-- ─────────────────────────────────────────────────────────────────────
-- Identifier parsing
-- ─────────────────────────────────────────────────────────────────────

-- | Namecoin name namespace.
--
-- @d\/foo@ is the domain namespace (publishes a Domain Name Object
-- with optional @nostr.names@ subtree); @id\/foo@ is the identity
-- namespace (publishes a single-identity record).
data Namespace = Domain | Identity
  deriving (Eq, Show)

-- | A user input parsed into the canonical lookup shape.
data ParsedNamecoinId = ParsedNamecoinId
  { parsedName      :: !Text
    -- ^ The full Namecoin name (e.g. @d\/example@, @id\/alice@).
  , parsedLocalPart :: !Text
    -- ^ The local-part within the name\'s value, or @"_"@ for the root.
  , parsedNamespace :: !Namespace
  } deriving (Eq, Show)

-- | True if the input should be routed to Namecoin resolution rather
-- than the standard NIP-05 HTTP path.
--
-- Accepts:
--
-- * @user\@something.bit@ (NIP-05 shape with @.bit@ TLD)
-- * @something.bit@ (bare @.bit@ domain → root local-part)
-- * @d\/name@, @id\/name@ (direct Namecoin name reference)
isNamecoinIdentifier :: Text -> Bool
isNamecoinIdentifier input =
  let lower = T.toLower (T.strip input)
  in    ".bit" `T.isSuffixOf` lower
     || "d/"  `T.isPrefixOf` lower
     || "id/" `T.isPrefixOf` lower

-- | Parse a user-supplied identifier into the canonical lookup shape,
-- mirroring 'NamecoinNameResolver.parseIdentifier' in the Kotlin
-- reference. Returns 'Nothing' if the input is not a recognised
-- Namecoin shape.
parseNamecoinIdentifier :: Text -> Maybe ParsedNamecoinId
parseNamecoinIdentifier raw =
  let input = T.toLower (T.strip raw)
  in if "d/" `T.isPrefixOf` input
       then Just (ParsedNamecoinId input "_" Domain)
       else if "id/" `T.isPrefixOf` input
         then Just (ParsedNamecoinId input "_" Identity)
         else if "@" `T.isInfixOf` input && ".bit" `T.isSuffixOf` input
           then case T.splitOn "@" input of
                  [lp, dom] ->
                    let localPart = if T.null lp then "_" else lp
                        domain    = T.dropEnd 4 dom  -- strip ".bit"
                    in if T.null domain
                         then Nothing
                         else Just (ParsedNamecoinId ("d/" <> domain) localPart Domain)
                  _ -> Nothing
           else if ".bit" `T.isSuffixOf` input
             then let domain = T.dropEnd 4 input
                  in if T.null domain
                       then Nothing
                       else Just (ParsedNamecoinId ("d/" <> domain) "_" Domain)
             else Nothing

-- ─────────────────────────────────────────────────────────────────────
-- ifa-0001 value parsing (pure)
-- ─────────────────────────────────────────────────────────────────────

-- | Pull a (pubkey, relays) pair out of a parsed Namecoin record
-- value, given the local-part the caller asked for.
--
-- Handles all three @nostr@ shapes from the Kotlin reference:
--
-- 1. String: @"nostr": "<hex>"@ (root only).
-- 2. Object with @names@ map: NIP-05-like.
-- 3. Object with bare @pubkey@: single-identity (root only).
--
-- For the Domain namespace, the shape priority is
-- @names[localPart]@ > @names["_"]@ > first @names@ entry (when root)
-- > bare @pubkey@. For the Identity namespace, @pubkey@ wins, then
-- @names["_"]@. This matches the Kotlin reference precedence.
extractNostrFromValue :: Namespace -> Text -> Value -> Maybe (Text, [Text])
extractNostrFromValue ns localPart (Object root) =
  case KeyMap.lookup "nostr" root of
    Just (String pkHex)
      | localPart == "_" && isValidHexPubkey pkHex ->
          Just (T.toLower pkHex, [])
    Just (Object nostrObj) ->
      case ns of
        Domain   -> domainCase nostrObj
        Identity -> identityCase nostrObj
    _ -> Nothing
  where
    -- Domain namespace: try names map first, fall back to single-identity
    -- only for the root local-part.
    domainCase nostrObj =
      let namesMb = KeyMap.lookup "names" nostrObj >>= asObject
      in case namesMb of
           Just names ->
             case lookupName localPart names of
               Just pk -> Just (T.toLower pk, relaysForPubkey nostrObj pk)
               Nothing
                 | localPart /= "_" -> Nothing
                 | otherwise ->
                     case lookupName "_" names of
                       Just pk  -> Just (T.toLower pk, relaysForPubkey nostrObj pk)
                       Nothing  ->
                         case firstNameEntry names of
                           Just pk -> Just (T.toLower pk, relaysForPubkey nostrObj pk)
                           Nothing -> singleIdentity nostrObj
           Nothing
             | localPart /= "_" -> Nothing
             | otherwise -> singleIdentity nostrObj

    -- Identity namespace: bare pubkey first, then names["_"].
    identityCase nostrObj =
      case singleIdentity nostrObj of
        Just r  -> Just r
        Nothing ->
          let namesMb = KeyMap.lookup "names" nostrObj >>= asObject
          in case namesMb of
               Just names -> case lookupName "_" names of
                               Just pk -> Just (T.toLower pk, relaysForPubkey nostrObj pk)
                               Nothing -> Nothing
               Nothing -> Nothing

    singleIdentity nostrObj =
      case KeyMap.lookup "pubkey" nostrObj of
        Just (String pk) | isValidHexPubkey pk ->
          Just (T.toLower pk, relaysArray (KeyMap.lookup "relays" nostrObj))
        _ -> Nothing

    relaysArray (Just (Array xs)) =
      [r | String r <- V.toList xs]
    relaysArray _ = []

    relaysForPubkey nostrObj pk =
      case KeyMap.lookup "relays" nostrObj of
        Just (Object relaysObj) ->
          let lookupExact = KeyMap.lookup (Key.fromText (T.toLower pk)) relaysObj
              lookupCased = KeyMap.lookup (Key.fromText pk) relaysObj
          in case lookupExact of
               Just (Array xs) -> [r | String r <- V.toList xs]
               _ -> case lookupCased of
                 Just (Array xs) -> [r | String r <- V.toList xs]
                 _ -> []
        _ -> []

    lookupName lp names =
      case KeyMap.lookup (Key.fromText lp) names of
        Just (String pk) | isValidHexPubkey pk -> Just pk
        _ -> Nothing

    firstNameEntry names =
      case mapMaybe asValidPubkey (KeyMap.toAscList names) of
        (pk:_) -> Just pk
        []     -> Nothing

    asValidPubkey (_, String pk) | isValidHexPubkey pk = Just pk
    asValidPubkey _ = Nothing

    asObject (Object o) = Just o
    asObject _          = Nothing
extractNostrFromValue _ _ _ = Nothing

isValidHexPubkey :: Text -> Bool
isValidHexPubkey t =
  T.length t == 64 && T.all isHexChar t
  where
    isHexChar c =
      (c >= '0' && c <= '9') ||
      (c >= 'a' && c <= 'f') ||
      (c >= 'A' && c <= 'F')

-- ─────────────────────────────────────────────────────────────────────
-- Namecoin name index script + Electrum scripthash
-- ─────────────────────────────────────────────────────────────────────

-- Namecoin opcodes (repurposed Bitcoin opcodes):
--   OP_NAME_UPDATE     = 0x53 (OP_3)
--   OP_NAME_FIRSTUPDATE = 0x52 (OP_2) -- not used for indexing
opNameUpdate, op2Drop, opDrop, opReturn :: Word8
opNameUpdate = 0x53
op2Drop      = 0x6d
opDrop       = 0x75
opReturn     = 0x6a

opPushdata1, opPushdata2 :: Word8
opPushdata1 = 0x4c
opPushdata2 = 0x4d

-- | Bitcoin-style push-data encoding for a bytestring payload.
pushData :: ByteString -> ByteString
pushData bs =
  let len = BS.length bs
  in if len < 0x4c
       then BS.cons (fromIntegral len) bs
       else if len <= 0xff
         then BS.concat
                [ BS.singleton opPushdata1
                , BS.singleton (fromIntegral len)
                , bs
                ]
         else BS.concat
                [ BS.singleton opPushdata2
                , BS.singleton (fromIntegral (len `mod` 256))
                , BS.singleton (fromIntegral (len `div` 256 `mod` 256))
                , bs
                ]

-- | Build the canonical Namecoin name index script for a name. This
-- is the script the ElectrumX name index hashes; we hash the same
-- script and ask the server for its history.
--
-- Format: @OP_NAME_UPDATE \<push name\> \<push empty\> OP_2DROP OP_DROP OP_RETURN@.
buildNameIndexScript :: Text -> ByteString
buildNameIndexScript name =
  BS.concat
    [ BS.singleton opNameUpdate
    , pushData (encodeUtf8 name)
    , pushData BS.empty
    , BS.singleton op2Drop
    , BS.singleton opDrop
    , BS.singleton opReturn
    ]

-- | Electrum protocol scripthash: SHA-256 of the script, byte-reversed,
-- lowercase hex-encoded.
electrumScriptHash :: ByteString -> Text
electrumScriptHash script =
  let digest = SHA256.hash script
      reversed = BS.reverse digest
  in case decodeUtf8' (B16.encode reversed) of
       Right t -> T.toLower t
       Left _  -> T.empty  -- unreachable: B16.encode always ASCII

-- ─────────────────────────────────────────────────────────────────────
-- ElectrumX wire (TCP + TLS)
-- ─────────────────────────────────────────────────────────────────────

-- | Build a JSON-RPC 2.0 request line (no trailing newline; the
-- connection writer appends @\\n@).
buildRpcRequest :: Int -> Text -> Value -> ByteString
buildRpcRequest reqId method params =
  BSL.toStrict (encode
    (object [ "id"      .= reqId
            , "jsonrpc" .= ("2.0" :: Text)
            , "method"  .= method
            , "params"  .= params
            ]))

-- | Resolve a Namecoin name against the given server: returns the raw
-- record value (the JSON string the chain stores) if a current
-- non-expired entry exists. Returns 'Nothing' for both "name not
-- found" and "server unreachable"; the caller treats those the same
-- way for a single server and only distinguishes when ALL servers
-- have been tried.
--
-- Wire steps:
--
-- 1. @server.version@ (handshake, discarded).
-- 2. @blockchain.scripthash.get_history@ for the name index script
--    hash. Empty history → name does not exist on this server.
-- 3. @blockchain.transaction.get@ with verbose=true for the most
--    recent tx → scan @vout@ entries for an output script starting
--    with 0x53 (NAME_UPDATE) or 0x52 (NAME_FIRSTUPDATE) and parse
--    the value push.
nameShow :: Text -> ElectrumxServer -> IO (Maybe Text)
nameShow name server = do
  mNested <- withElectrumX server (queryConn name)
  return (case mNested of
            Just (Just v) -> Just v
            _             -> Nothing)

-- | Pure-on-the-wire portion of 'nameShow', extracted so the bracketed
-- TLS handle in 'withElectrumX' has a clean exit point and the
-- IO-level nesting (timeout / try / bracket) stays out of the
-- protocol logic.
queryConn :: Text -> Connection -> IO (Maybe Text)
queryConn name conn = do
  -- 1. Negotiate protocol version
  sendLine conn (buildRpcRequest 0 "server.version"
                  (toJSONList ["futr/0.5", "1.4"]))
  _ <- recvLine conn  -- discard version response

  -- 2. Query name index history
  let script     = buildNameIndexScript name
      scripthash = electrumScriptHash script
  sendLine conn (buildRpcRequest 1 "blockchain.scripthash.get_history"
                  (toJSONList [scripthash]))
  histResp <- recvLine conn
  case parseHistory histResp of
    Nothing -> return Nothing      -- error response
    Just [] -> return Nothing      -- name not found on this server
    Just entries -> do
      let (txHash, _height) = last entries
      -- 3. Fetch verbose transaction
      sendLine conn (buildRpcRequest 2 "blockchain.transaction.get"
                      (Array (V.fromList [String txHash, Bool True])))
      txResp <- recvLine conn
      return (parseValueFromTx name txResp)
  where
    toJSONList :: [Text] -> Value
    toJSONList = Array . V.fromList . map String

-- | Withdraw a TLS-wrapped ElectrumX connection for the duration of
-- the action.
withElectrumX :: ElectrumxServer -> (Connection -> IO a) -> IO (Maybe a)
withElectrumX (ElectrumxServer host port) action = do
  res <- tryAny $ timeout connectTimeoutMicros $ do
    ctx <- initConnectionContext
    bracket
      (connectTo ctx (mkParams host port))
      connectionClose
      action
  case res of
    Left _         -> return Nothing
    Right Nothing  -> return Nothing    -- timed out
    Right (Just r) -> return (Just r)
  where
    -- Inline alias so the inner try has a concrete exception type
    -- without needing ScopedTypeVariables on the outer signature.
    tryAny :: IO x -> IO (Either SomeException x)
    tryAny = try

    -- 20s total budget for the full handshake + 3 round trips. The
    -- ElectrumX servers in defaultElectrumxServers are public and
    -- often slow on first connect; tighter timeouts cause spurious
    -- "name not found" verdicts under cold-cache conditions.
    connectTimeoutMicros = 20 * 1000 * 1000

mkParams :: Text -> Int -> ConnectionParams
mkParams host port = ConnectionParams
  { connectionHostname  = T.unpack host
  , connectionPort      = fromIntegral port
    -- Use the package default TLS settings (validation on, system
    -- trust store, SNI on, sessions on). Defining this with
    -- positional TLSSettingsSimple fields would break cross-version
    -- compatibility because newer crypton-connection (0.4.x) adds a
    -- settingClientSupported field that 0.3.x doesn't have. 'def'
    -- always picks the right default for the linked version.
  , connectionUseSecure = Just def
  , connectionUseSocks  = Nothing
  }

sendLine :: Connection -> ByteString -> IO ()
sendLine conn payload = connectionPut conn (payload `BS.snoc` 0x0a)

-- | Receive one line (LF-terminated). connectionGetLine strips the LF.
-- The 256 KiB cap matches what other ElectrumX clients use and
-- comfortably exceeds the largest plausible @blockchain.transaction.get@
-- response for a Namecoin name tx.
recvLine :: Connection -> IO ByteString
recvLine conn = connectionGetLine (256 * 1024) conn

-- ─────────────────────────────────────────────────────────────────────
-- Wire-response parsing
-- ─────────────────────────────────────────────────────────────────────

-- | Parse @[{"tx_hash": ..., "height": ...}, ...]@ out of a
-- @blockchain.scripthash.get_history@ envelope.
parseHistory :: ByteString -> Maybe [(Text, Int)]
parseHistory raw = do
  envelope <- decode (BSL.fromStrict raw) :: Maybe Value
  case envelope of
    Object obj -> do
      -- Error envelopes have an "error" field that is non-null.
      case KeyMap.lookup "error" obj of
        Just Null     -> proceed obj
        Nothing       -> proceed obj
        Just _        -> Nothing  -- error response
    _ -> Nothing
  where
    proceed obj = case KeyMap.lookup "result" obj of
      Just (Array xs) -> Just (mapMaybe entry (V.toList xs))
      _               -> Nothing
    entry (Object o) = do
      String h <- KeyMap.lookup "tx_hash" o
      Number n <- KeyMap.lookup "height" o
      height   <- toBoundedInteger n :: Maybe Int
      return (h, height)
    entry _ = Nothing

-- | Walk the @vout@ array of a verbose-transaction response, returning
-- the value of the first output whose script encodes a NAME_UPDATE or
-- NAME_FIRSTUPDATE for the requested name.
parseValueFromTx :: Text -> ByteString -> Maybe Text
parseValueFromTx wantedName raw = do
  envelope <- decode (BSL.fromStrict raw) :: Maybe Value
  case envelope of
    Object obj -> do
      case KeyMap.lookup "error" obj of
        Just Null     -> proceed obj
        Nothing       -> proceed obj
        Just _        -> Nothing
    _ -> Nothing
  where
    proceed obj = do
      Object result <- KeyMap.lookup "result" obj
      Array vouts   <- KeyMap.lookup "vout" result
      listToMaybe (mapMaybe scanVout (V.toList vouts))
    scanVout (Object v) = do
      Object spk <- KeyMap.lookup "scriptPubKey" v
      String hex <- KeyMap.lookup "hex" spk
      bytes <- hexDecode hex
      (nm, val) <- parseNameScript bytes
      if nm == wantedName then Just val else Nothing
    scanVout _ = Nothing

-- | Decode a Namecoin NAME_UPDATE or NAME_FIRSTUPDATE output script
-- into @(name, value)@.
--
-- @NAME_UPDATE@:     @OP_3 \<name\> \<value\> OP_2DROP OP_DROP \<address\>@
-- @NAME_FIRSTUPDATE@: @OP_2 \<name\> \<rand\> \<value\> OP_2DROP OP_2DROP \<address\>@
parseNameScript :: ByteString -> Maybe (Text, Text)
parseNameScript bs = do
  (op, pos1) <- readByte bs 0
  guard (op == 0x53 || op == 0x52)
  (nameBytes, pos2) <- readPushData bs pos1
  pos3 <- if op == 0x52
            then do
              (_rand, p) <- readPushData bs pos2
              return p
            else return pos2
  (valueBytes, _) <- readPushData bs pos3
  nm  <- eitherToMaybe (decodeUtf8' nameBytes)
  val <- eitherToMaybe (decodeUtf8' valueBytes)
  return (nm, val)
  where
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _)  = Nothing

readByte :: ByteString -> Int -> Maybe (Word8, Int)
readByte bs pos
  | pos < BS.length bs = Just (BS.index bs pos, pos + 1)
  | otherwise          = Nothing

readPushData :: ByteString -> Int -> Maybe (ByteString, Int)
readPushData bs pos = do
  (op, p1) <- readByte bs pos
  case fromIntegral op :: Int of
    0   -> Just (BS.empty, p1)
    n | n < 0x4c -> takeN bs p1 n
    0x4c -> do
      (lenByte, p2) <- readByte bs p1
      takeN bs p2 (fromIntegral lenByte)
    0x4d -> do
      (lo, p2) <- readByte bs p1
      (hi, p3) <- readByte bs p2
      let len = fromIntegral lo + 256 * fromIntegral hi :: Int
      takeN bs p3 len
    _ -> Nothing
  where
    takeN buf start n
      | start + n <= BS.length buf =
          Just (BS.take n (BS.drop start buf), start + n)
      | otherwise = Nothing

hexDecode :: Text -> Maybe ByteString
hexDecode t = case B16.decode (encodeUtf8 t) of
  Right bs -> Just bs
  Left _   -> Nothing

guard :: Bool -> Maybe ()
guard True  = Just ()
guard False = Nothing

-- ─────────────────────────────────────────────────────────────────────
-- Public entry point
-- ─────────────────────────────────────────────────────────────────────

-- | Resolve a @.bit@ identifier to a Nostr pubkey via Namecoin, using
-- the default ElectrumX server list. Returns 'Nothing' for any
-- combination of "all servers unreachable", "name not found", or
-- "name exists but has no nostr field" — futr surfaces these the same
-- way already for HTTP NIP-05 misses.
--
-- The result is a bare @(pubkey, relays)@ pair so this module can
-- live below 'Nostr.Nip05Search' in the module DAG without an import
-- cycle. The caller wraps it into the project-wide
-- 'Nostr.Nip05Search.Nip05SearchResult'.
searchNamecoinNip05 :: Text -> IO (Maybe (PubKeyXO, [Text]))
searchNamecoinNip05 identifier =
  case parseNamecoinIdentifier identifier of
    Nothing -> return Nothing
    Just parsed -> do
      mValue <- tryServers (parsedName parsed) defaultElectrumxServers
      case mValue of
        Nothing -> return Nothing
        Just valueText ->
          case eitherDecodeStrict (encodeUtf8 valueText) of
            Left _      -> return Nothing
            Right value ->
              case extractNostrFromValue (parsedNamespace parsed)
                                         (parsedLocalPart parsed) value of
                Nothing             -> return Nothing
                Just (pkHex, relays) -> case pubKeyXOFromHex pkHex of
                  Nothing -> return Nothing
                  Just pk -> return (Just (pk, relays))

-- | Try each server in order; return the first successful name value.
-- A "successful" lookup is one that returned a (name, value) pair on
-- the chain; servers that fail to connect or return empty history are
-- skipped without surfacing the difference to the caller.
tryServers :: Text -> [ElectrumxServer] -> IO (Maybe Text)
tryServers _    []       = return Nothing
tryServers name (s:rest) = do
  r <- nameShow name s
  case r of
    Just v  -> return (Just v)
    Nothing -> tryServers name rest
