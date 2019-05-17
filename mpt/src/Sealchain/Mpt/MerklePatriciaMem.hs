
module Sealchain.Mpt.MerklePatriciaMem (
  putKeyValMem, getKeyValMem, deleteKeyMem, keyExistsMem,
  initializeBlankMem,
  MPMem(..)
  ) where

import           Universum
import qualified Universum.Unsafe as Unsafe

import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto
import qualified Data.Map as Map
import           Data.Maybe(isJust)

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.NodeData
import           Sealchain.Mpt.MerklePatricia.InternalMem
import           Sealchain.Mpt.MerklePatricia.Utils

putKeyValMem::Monad m=>MPMem
           ->MPKey
           ->MPVal
           ->m MPMem
putKeyValMem db = unsafePutKeyValMem db . keyToSafeKey


getKeyValMem::Monad m=>MPMem
         -> MPKey
         -> m (Maybe MPVal)
getKeyValMem db key = do
  vals <- unsafeGetKeyValsMem db (keyToSafeKey key)
  return $
    if not (null vals)
    then Just $ snd (Unsafe.head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

deleteKeyMem::Monad m=>MPMem
         ->MPKey
         ->m MPMem
deleteKeyMem db = unsafeDeleteKeyMem db . keyToSafeKey

keyExistsMem::Monad m=>MPMem
         ->MPKey
         ->m Bool
keyExistsMem db key = isJust <$> getKeyValMem db key


initializeBlankMem ::MPMem
initializeBlankMem =
    let theRLP = rlpEncode (0::Integer)
        bytes = rlpSerialize theRLP
        key = convert $ (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)
    in
      MPMem {
        mpMap  = Map.insert key bytes Map.empty,
        mpRoot = bytes
      }


