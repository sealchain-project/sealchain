{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.Utils (
  keyToSafeKey,
  getCommonPrefix,
  replace,
  prependToKey,
  slotIsEmpty,
  list2Options,
  options2List
  ) where

import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto
import qualified Data.NibbleString as N

import           Sealchain.Mpt.MerklePatricia.NodeData

keyToSafeKey::N.NibbleString->N.NibbleString
keyToSafeKey key =
    N.EvenNibbleString $ convert $ (Crypto.hash keyByteString :: Crypto.Digest Crypto.Keccak_256)
  where
    N.EvenNibbleString keyByteString = key

list2Options::N.Nibble->[(N.Nibble, NodeRef)]->[NodeRef]
list2Options start [] = replicate (fromIntegral $ 0x10 - start) emptyRef
list2Options start x | start > 15 =
  error $
  "value of 'start' in list2Option is greater than 15, it is: " ++ show start
  ++ ", second param is " ++ show x
list2Options start ((firstNibble, firstPtr):rest) =
    replicate (fromIntegral $ firstNibble - start) emptyRef ++ [firstPtr] ++ list2Options (firstNibble+1) rest

options2List::[NodeRef]->[(N.Nibble, NodeRef)]
options2List theList = filter ((/= emptyRef) . snd) $ zip [0..] theList

prependToKey::Key->(Key, Val)->(Key, Val)
prependToKey prefix (key, val) = (prefix `N.append` key, val)

replace::Integral i=>[a]->i->a->[a]
replace lst i newVal = left ++ [newVal] ++ right
            where
              (left, _:right) = splitAt (fromIntegral i) lst

slotIsEmpty::[NodeRef]->N.Nibble->Bool
slotIsEmpty [] _ =
  error "slotIsEmpty was called for value greater than the size of the list"
slotIsEmpty (x:_) 0 | x == emptyRef = True
slotIsEmpty _ 0 = False
slotIsEmpty (_:rest) n = slotIsEmpty rest (n-1)


getCommonPrefix::Eq a=>[a]->[a]->([a], [a], [a])
getCommonPrefix (c1:rest1) (c2:rest2)
  | c1 == c2 = prefixTheCommonPrefix c1 (getCommonPrefix rest1 rest2)
  where
    prefixTheCommonPrefix c (p, x, y) = (c:p, x, y)
getCommonPrefix x y = ([], x, y)
