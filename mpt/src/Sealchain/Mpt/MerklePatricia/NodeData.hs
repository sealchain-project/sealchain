{-# LANGUAGE OverloadedStrings #-}

module Sealchain.Mpt.MerklePatricia.NodeData (
  Key,
  Val,
  NodeData(..),
  NodeRef(..),
  emptyRef,
  formatNodeRef
  ) where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Internal
import qualified Data.NibbleString as N
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Blockchain.Data.RLP

-------------------------

-- | The type of the database key
type Key = N.NibbleString

-- | The type of the values in the database
type Val = RLPObject

-------------------------

data NodeRef = SmallRef B.ByteString | PtrRef B.ByteString deriving (Show, Eq)

emptyRef::NodeRef
emptyRef = SmallRef $ B.pack [0x80]

formatNodeRef :: NodeRef -> String
formatNodeRef (SmallRef bs) = T.unpack .  decodeUtf8 . B16.encode $ bs
formatNodeRef (PtrRef bs)   = T.unpack .  decodeUtf8 . B16.encode $ bs

instance Pretty NodeRef where
  pretty = text . formatNodeRef

-------------------------

data NodeData = EmptyNodeData
              | FullNodeData {
                 -- Why not make choices a map (choices::M.Map N.Nibble NodeRef)?  Because this type tends to be created
                 -- more than items are looked up in it....  It would actually slow things down to use it.
                 choices :: [NodeRef],
                 nodeVal :: Maybe Val
                }
              | ShortcutNodeData {
                  nextNibbleString :: Key,
                  nextVal          :: Either NodeRef Val
                }
              deriving (Show, Eq)

formatVal::Maybe RLPObject->Doc
formatVal Nothing  = red $ text "NULL"
formatVal (Just x) = green $ pretty x

instance Pretty NodeData where
  pretty EmptyNodeData = text "    <EMPTY>"
  pretty (ShortcutNodeData s (Left p)) = text $ "    " ++ show (pretty s) ++ " -> " ++ show (pretty p)
  pretty (ShortcutNodeData s (Right val)) = text $ "    " ++ show (pretty s) ++ " -> " ++ show (green $ pretty val)
  pretty (FullNodeData cs val) = text "    val: " </> formatVal val </> text "\n        " </> vsep (showChoice <$> zip ([0..]::[Int]) cs)
    where
      showChoice::(Int, NodeRef)->Doc
      showChoice (v, SmallRef "") = blue (text $ showHex v "") </> text ": " </> red (text "NULL")
      showChoice (v, p)           = blue (text $ showHex v "") </> text ": " </> green (pretty p)

instance RLPSerializable NodeData where
  rlpEncode EmptyNodeData = RLPString ""
  rlpEncode (FullNodeData {choices=cs, nodeVal=val}) = RLPArray ((encodeChoice <$> cs) ++ [encodeVal val])
    where
      encodeChoice::NodeRef->RLPObject
      encodeChoice (SmallRef "")          = rlpEncode (0::Integer)
      encodeChoice (PtrRef x)             = rlpEncode x
      encodeChoice (SmallRef o)           = rlpDeserialize o
      encodeVal::Maybe Val->RLPObject
      encodeVal Nothing  = rlpEncode (0::Integer)
      encodeVal (Just x) = x
  rlpEncode (ShortcutNodeData {nextNibbleString=s, nextVal=val}) =
    RLPArray[rlpEncode $ BC.unpack $ termNibbleString2String terminator s, encodeVal val]
    where
      terminator =
        case val of
          Left _  -> False
          Right _ -> True
      encodeVal::Either NodeRef Val->RLPObject
      encodeVal (Left (PtrRef x))   = rlpEncode x
      encodeVal (Left (SmallRef x)) = rlpDeserialize x
      encodeVal (Right x)           = x

  rlpDecode (RLPString "") = EmptyNodeData
  rlpDecode (RLPScalar 0) = EmptyNodeData
  rlpDecode (RLPArray [a, val])
      | terminator = ShortcutNodeData s $ Right val
      | B.length (rlpSerialize val) >= 32 =
          ShortcutNodeData s (Left $ PtrRef (BC.pack $ rlpDecode val))
      | otherwise =
          ShortcutNodeData s (Left $ SmallRef $ rlpSerialize val)
    where
      (terminator, s) = string2TermNibbleString $ rlpDecode a
  rlpDecode (RLPArray x) | length x == 17 =
    FullNodeData (getPtr <$> childPointers) val
    where
      childPointers = init x
      val = case last x of
        RLPScalar 0  -> Nothing
        RLPString "" -> Nothing
        x'           -> Just x'
      getPtr::RLPObject->NodeRef
      getPtr o | B.length (rlpSerialize o) < 32 = SmallRef $ rlpSerialize o
      --getPtr o@(RLPArray [_, _]) = SmallRef $ rlpSerialize o
      getPtr p = PtrRef $ rlpDecode p
  rlpDecode x = error ("Missing case in rlpDecode for NodeData: " ++ show x)

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
