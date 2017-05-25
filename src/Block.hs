{-# LANGUAGE DeriveGeneric #-}

module Block (
    Block,
    BlockBuilder,
    genesis,
    newBB,
    prevBlock,
    currentDat,
    size,
    blockHash,
    dat,
    addRec,
    createNewBlock
) where

import Crypto.Hash as Cryp
import Time.System
import Time.Types
import GHC.Generics
import Data.Aeson
import Data.Bson.Generic
import System.IO.Unsafe
import Data.Maybe
import Dat
import qualified Data.ByteString.Char8 as C8

data Block = Block {
    index :: !Integer,
    prevHash :: !String,
    timestamp :: !String,
    dat :: !Dat,
    blockHash :: !String
}
    deriving (Generic, Show, Eq)

instance ToJSON Block
instance FromJSON Block

instance ToBSON Block
instance FromBSON Block

calculateHash :: Integer -> String -> String -> Dat -> Hash
calculateHash index prevH timestamp dat =
    let
        content = concat [show index,prevH,timestamp,show dat]
        raw_content = C8.pack content
    in
        Cryp.hash raw_content

genesis :: Block
genesis =
  let
    ind = 0
    dummyHash = hash $ C8.pack "a" :: Hash
    hashSize = div (length $ show dummyHash) 2
    pH  = (fromJust . digestFromByteString) . C8.pack $ replicate hashSize '\0' :: Hash
    ts  = show $ unsafePerformIO timeCurrent
    d   = newDat
  in
  Block {
    index = ind,
    prevHash = show pH,
    timestamp = ts,
    dat = d ,
    blockHash = show $ calculateHash ind (show pH) ts d
  }

data BlockBuilder = BlockBuilder {
    prevBlock :: Block,
    currentDat :: Dat,
    size :: Int
}
  deriving (Show)

newBB :: Block -> Int -> BlockBuilder
newBB b s
  | s<=0 =  BlockBuilder { prevBlock = b, currentDat = newDat, size = 1}
  | otherwise =  BlockBuilder { prevBlock = b, currentDat = newDat, size = s}

addRec :: Record -> BlockBuilder -> Either BlockBuilder Block
addRec rec bb =
  let
    sz = size bb
    currDat = currentDat bb
    currsz = numTransactions currDat
    updated_dat = addRecord rec currDat
    updated_bb = BlockBuilder{prevBlock = prevBlock bb, currentDat = updated_dat, size = size bb}
  in 
    if sz == (currsz+1) then Right (createNewBlock updated_dat (prevBlock bb)) else Left updated_bb

createNewBlock :: Dat -> Block -> Block
createNewBlock d prev_block =
  let
      ind = (+1) $ index prev_block
      pH  = blockHash prev_block
      ts  = show $ unsafePerformIO timeCurrent
      newHash = calculateHash ind pH ts d
  in
      Block {index = ind, prevHash = pH, timestamp = ts, dat = d, blockHash = show newHash}
