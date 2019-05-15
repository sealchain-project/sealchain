{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia
import           Blockchain.Database.MerklePatricia.Internal
import           Blockchain.Database.MerklePatricia.InternalMem
import           Blockchain.Database.MerklePatricia.MPDB
import           Blockchain.Database.MerklePatriciaMem
import           Control.Monad.Trans.Resource
import qualified Data.NibbleString                              as N
import qualified Database.LevelDB                               as LD
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                       (fromHUnitTest)
import           Test.HUnit

bigTest :: [(Key,String)]
bigTest=
  [
    ("00000000000000000000000000000000ffffffffffffffff0000000000000000", "90467269656e647320262046616d696c79"),
    ("00000000000000000000000000000000ffffffffffffffff0000000000000001", "8772656631323334"),
    ("00000000000000000000000000000000ffffffffffffffff0000000000000002", "04"),
    ("00000000000000000000000000000000ffffffffffffffff0000000000000003", "84548123a8"),
    ("0000000000000000000000000000000000000000000000000000000000000000", "974c696162696c69746965733a496e697469616c4c6f616e"),
    ("0000000000000000000000000000000000000000000000000000000000000001", "a0fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe7960"),
    ("0000000000000000000000000000000000000000000000000000000000000002", "83555344"),
    ("0000000000000000000000000000000000000000000000010000000000000000", "8f4173736574733a436865636b696e67"),
    ("0000000000000000000000000000000000000000000000010000000000000001", "830186a0"),
    ("0000000000000000000000000000000000000000000000010000000000000002", "83555344"),
    ("00000000000000000000000000000002ffffffffffffffff0000000000000003", "84548123a8")
  ]

addAllKVs::RLPSerializable obj=>MonadResource m=>MPDB->[(N.NibbleString, obj)]->m MPDB
addAllKVs x [] = return x
addAllKVs mpdb (x:rest) = do
  mpdb' <- unsafePutKeyVal mpdb (fst x) (rlpEncode $ rlpSerialize $ rlpEncode $ snd x)
  addAllKVs mpdb' rest

addAllKVsMem::RLPSerializable obj=>Monad m=>MPMem->[(N.NibbleString, obj)]->m MPMem
addAllKVsMem x [] = return x
addAllKVsMem mpdb (x:rest) = do
  mpdb' <- unsafePutKeyValMem mpdb (fst x) (rlpEncode $ rlpSerialize $ rlpEncode $ snd x)
  addAllKVsMem mpdb' rest

blank :: MPMem
blank = initializeBlankMem {mpStateRoot=emptyTriePtr}

testGetPut :: Test
testGetPut = TestCase $ do
  db <- putSingleKV key val
  res <- getSingleKV db key

  assertEqual "get . put = id" res [(key,val)]

testGetPutRepeated :: Test
testGetPutRepeated = TestCase $ do
  db <- putSingleKV key val
  db2 <- unsafePutKeyValMem db key2 val2

  res <- getSingleKV db2 key2

  assertEqual "get . put . put = id" res [(key2,val2)]

testGetPutRepeatedII :: Test
testGetPutRepeatedII = TestCase $ do
  db <- addAllKVsMem blank bigTest

  res <- getSingleKV db "00000000000000000000000000000002ffffffffffffffff0000000000000003"

  assertEqual "get . putn = id" res [("00000000000000000000000000000002ffffffffffffffff0000000000000003",rlpEncode $ rlpSerialize $ rlpEncode ("84548123a8" :: String))]

testSingleInsert :: Test
testSingleInsert = TestCase $ do
  sr <- runResourceT $ do
      db <- LD.open "/tmp/testDB" LD.defaultOptions{LD.createIfMissing=True}

      let ldb' = MPDB {ldb=db,stateRoot=emptyTriePtr}

      initializeBlank ldb'

      addAllKVs ldb' [head bigTest]

  sr2 <- addAllKVsMem blank [head bigTest]

  assertEqual "disk - mem single insert" (stateRoot sr) (mpStateRoot sr2)


testMultipleInserts :: Test
testMultipleInserts = TestCase $ do
  sr <- runResourceT $ do
      db <- LD.open "/tmp/testDB2" LD.defaultOptions{LD.createIfMissing=True}

      let ldb' = MPDB {ldb=db,stateRoot=emptyTriePtr}

      initializeBlank ldb'

      addAllKVs ldb' bigTest

  sr2 <- addAllKVsMem blank bigTest

  assertEqual "disk - mem multiple insert" (stateRoot sr) (mpStateRoot sr2)


key :: N.NibbleString
key = (N.EvenNibbleString "anyString")

val :: RLPObject
val = (RLPString "anotherString")

key2 :: N.NibbleString
key2 = (N.EvenNibbleString "otherString")

val2 :: RLPObject
val2 = (RLPString "thatString2")

putSingleKV :: (Monad m) => Key->Val->m MPMem
putSingleKV k v= unsafePutKeyValMem blank k v

getSingleKV :: (Monad m) => MPMem -> Key -> m [(Key,Val)]
getSingleKV db key' = unsafeGetKeyValsMem db key'

spec :: Spec
spec = do
  describe "the old merkle-patricia test suite" $ do
       fromHUnitTest $ TestList [TestLabel " get . put = id" testGetPut,
                                 TestLabel " get . put . put = id" testGetPutRepeated,
                                 TestLabel " get . putn = id" testGetPutRepeatedII,
                                 TestLabel " single insert" testSingleInsert,
                                 TestLabel " multiple insert" testMultipleInserts]

main :: IO ()
main = hspec spec
