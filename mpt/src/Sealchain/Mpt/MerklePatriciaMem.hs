
module Sealchain.Mpt.MerklePatriciaMem (
  putKeyValMem, getKeyValMem, deleteKeyMem, keyExistsMem,
  initializeBlankMem,
  MPMem(..)
  ) where

import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto
import qualified Data.Map as Map
import           Data.Maybe(isJust)

import           Blockchain.Data.RLP
import           Sealchain.Mpt.MerklePatricia.InternalMem
import           Sealchain.Mpt.MerklePatricia.StateRoot

putKeyValMem::Monad m=>MPMem
           ->Key
           ->Val
           ->m MPMem
putKeyValMem db = unsafePutKeyValMem db . keyToSafeKeyMem


getKeyValMem::Monad m=>MPMem
         -> Key
         -> m (Maybe Val)
getKeyValMem db key = do
  vals <- unsafeGetKeyValsMem db (keyToSafeKeyMem key)
  return $
    if not (null vals)
    then Just $ snd (head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

deleteKeyMem::Monad m=>MPMem
         ->Key
         ->m MPMem
deleteKeyMem db = unsafeDeleteKeyMem db . keyToSafeKeyMem

keyExistsMem::Monad m=>MPMem
         ->Key
         ->m Bool
keyExistsMem db key = isJust <$> getKeyValMem db key


initializeBlankMem ::MPMem
initializeBlankMem =
    let theRLP = rlpEncode (0::Integer)
        bytes = rlpSerialize theRLP
        key = convert $ (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)
    in
      MPMem {
        mpMap = Map.insert key bytes Map.empty,
        mpStateRoot = StateRoot bytes
      }


