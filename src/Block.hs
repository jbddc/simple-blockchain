module Block (
    Block,
    genesis,
    createNewBlock
) where

import Crypto.Hash as Cryp
import Time.System
import Time.Types
import System.IO.Unsafe
import Data.Maybe
import Dat
import qualified Data.ByteString.Char8 as C8

data Block = Block { 
    index :: Integer,
    prevHash :: Hash,
    timestamp :: String,
    dat :: Dat,
    blockHash :: Hash
}
    deriving (Show, Eq)

calculateHash :: Integer -> Hash -> String -> Dat -> Hash
calculateHash index prevH timestamp dat = 
    let
        content = concat [show index,show prevH,timestamp,show dat]
        raw_content = C8.pack content
    in
        Cryp.hash raw_content

genesis = 
  let
    ind = 0
    dummyHash = hash $ C8.pack "a" :: Hash
    hashSize = div (length $ show dummyHash) 2
    pH  = fromJust $ digestFromByteString $ C8.pack $ take hashSize $ repeat '\0' :: Hash
    ts  = show $ unsafePerformIO timeCurrent
    d   = newDat
  in 
  Block {
    index = ind,
    prevHash = pH,
    timestamp = ts,
    dat = d ,
    blockHash = calculateHash ind pH ts d 
  }

createNewBlock :: Dat -> Block -> Block
createNewBlock d prev_block =
  let
      ind = (+1) $ index prev_block
      pH  = blockHash prev_block
      ts  = show $ unsafePerformIO timeCurrent 
      newHash = calculateHash ind pH ts d
  in
      Block {index = ind, prevHash = pH, timestamp = ts, dat = d, blockHash = newHash} 