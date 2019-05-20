{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sealchain.Mpt.MerklePatricia.StateRoot (
  StateRoot(..),
  emptyTriePtr,
  sha2StateRoot,
  unboxStateRoot,
  formatStateRoot,
  ) where

import           Universum

import qualified Data.ByteString.Base16 as B16
import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto


import qualified Data.ByteString as B
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Sealchain.Mpt.MerklePatricia.NodeData

-- | Internal nodes are indexed in the underlying database by their 256-bit SHA3 hash.
-- This types represents said hash.
--
-- The stateRoot is of this type,
-- (ie- the pointer to the full set of key/value pairs at a particular time in history), and
-- will be of interest if you need to refer to older or parallel version of the data.

newtype StateRoot = StateRoot MPPtr deriving (Show, Eq, Read, Generic, IsString)

formatStateRoot :: StateRoot -> String
formatStateRoot (StateRoot sr) = T.unpack .  decodeUtf8 . B16.encode $ sr

instance Pretty StateRoot where
  pretty = text . formatStateRoot

instance Bi StateRoot where
  encode (StateRoot bs) = Bi.encode bs 
  decode = StateRoot <$> Bi.decode

-- | The stateRoot of the empty database.
emptyTriePtr::StateRoot
emptyTriePtr =
  let root = (0::Integer)
      rootHash = convert $ (Crypto.hash . Bi.serialize' $ root :: Crypto.Digest Crypto.Keccak_256)
  in StateRoot rootHash

sha2StateRoot::Digest Crypto.Keccak_256 -> StateRoot
sha2StateRoot = StateRoot . convert

unboxStateRoot :: StateRoot -> B.ByteString
unboxStateRoot (StateRoot b) = b
