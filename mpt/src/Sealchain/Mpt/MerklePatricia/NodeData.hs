{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sealchain.Mpt.MerklePatricia.NodeData (
  MPKey,
  MPVal,
  MPPtr,
  NodeData(..),
  NodeRef(..),
  emptyRef,
  formatNodeRef
  ) where

import           Universum

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Internal
import qualified Data.NibbleString as N

import           Pos.Binary.Class

-------------------------

-- | The type of the database key
type MPKey = N.NibbleString

-- | The type of the values in the database
type MPVal = B.ByteString

-- | The type of the ptr in the database
type MPPtr = B.ByteString

-------------------------

data NodeRef = SmallRef B.ByteString | PtrRef MPPtr deriving (Show, Eq)

emptyRef::NodeRef
emptyRef = SmallRef $ B.pack [0x80]

formatNodeRef :: NodeRef -> Text
formatNodeRef (SmallRef bs) = decodeUtf8 . B16.encode $ bs
formatNodeRef (PtrRef bs)   = decodeUtf8 . B16.encode $ bs

-------------------------

data NodeData = EmptyNodeData
              | FullNodeData {
                 -- Why not make choices a map (choices::M.Map N.Nibble NodeRef)?  Because this type tends to be created
                 -- more than items are looked up in it....  It would actually slow things down to use it.
                 choices :: [NodeRef],
                 nodeVal :: Maybe MPVal
                }
              | ShortcutNodeData {
                  nextNibbleString :: MPKey,
                  nextVal          :: Either NodeRef MPVal
                }
              deriving (Show, Eq)

instance Bi NodeRef where
  encode x = case x of
        (SmallRef b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (PtrRef s) -> encodeListLen 2 <> encode (1 :: Word8) <> encode s
  decode = do
      enforceSize "NodeRef" 2
      tag <- decode @Word8
      case tag of
          0 -> SmallRef <$> decode
          1 -> PtrRef <$> decode
          _ -> cborError "Found invalid tag while getting NodeRef" 

instance Bi NodeData where
  encode x = case x of 
      (EmptyNodeData) ->
        encodeListLen 3 <> encode (0 :: Word8) <> encode () <> encode ()
      (FullNodeData {choices=cs, nodeVal=val}) ->
        encodeListLen 3 <> encode (1 :: Word8) <> encode cs <> encode val
      (ShortcutNodeData {nextNibbleString=s, nextVal=val}) ->
        encodeListLen 3 <> encode (2 :: Word8) <> encode (BC.unpack (termNibbleString2String terminator s))<> encode val
        where 
          terminator =
            case val of
              Left _  -> False
              Right _ -> True
  decode = do
      enforceSize "NodeData" 3
      tag <- decode @Word8
      case tag of 
        0 -> do
             _ <- decode @() 
             _ <- decode @()
             return EmptyNodeData
        1 -> do
             cs <- decode
             val <- decode
             return $ FullNodeData cs val
        2 -> do
             ns <- decode
             val <- decode
             let (_, s) =  string2TermNibbleString ns
             return  $ ShortcutNodeData s val
        _ -> cborError "Found invalid tag while getting NodeData"     
  

string2TermNibbleString::String->(Bool, N.NibbleString)
string2TermNibbleString [] = error "string2TermNibbleString called with empty String"
string2TermNibbleString (c:rest) =
  (terminator, s)
  where
    w = c2w c
    (flags, extraNibble) = if w > 0xF then (w `shiftR` 4, 0xF .&. w) else (w, 0)
    terminator = flags `shiftR` 1 == 1
    oddLength = flags .&. 1 == 1
    s = if oddLength then N.OddNibbleString extraNibble (BC.pack rest) else N.EvenNibbleString (BC.pack rest)

termNibbleString2String::Bool->N.NibbleString->B.ByteString
termNibbleString2String terminator s =
  case s of
    (N.EvenNibbleString s')    -> B.singleton (extraNibble `shiftL` 4) `B.append` s'
    (N.OddNibbleString n rest) -> B.singleton (extraNibble `shiftL` 4 + n) `B.append` rest
  where
    extraNibble =
        (if terminator then 2 else 0) +
        (if odd $ N.length s then 1 else 0)