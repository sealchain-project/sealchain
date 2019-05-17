{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.Utils 
  ( byteStringToSafeKey
  ) where

import           Universum

import qualified Data.NibbleString as N
import           Data.ByteArray(convert)
import           Crypto.Hash as Crypto

import Sealchain.Mpt.MerklePatricia.NodeData

byteStringToSafeKey :: ByteString -> MPKey
byteStringToSafeKey bs =
    N.EvenNibbleString $ convert $ (Crypto.hash bs :: Crypto.Digest Crypto.Keccak_256)