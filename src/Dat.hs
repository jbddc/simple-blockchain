{-# LANGUAGE DeriveGeneric #-}
module Dat where


import qualified Data.Map.Strict as Map
import Time.Types
import Data.Aeson
import GHC.Generics
import Crypto.Hash as Cryp
import qualified Data.ByteString.Char8 as C8

type Hash = Digest SHA256
type Dat = Map.Map String Transaction

data Transaction = T {
    fromAddr :: String
    , toAddr :: String
    , value :: Double
    , message :: String
    , tstamp :: String
    , chksum :: String
}
 deriving (Generic,Show,Eq)

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Transaction

newDat :: Dat
newDat = Map.empty

addTrans :: Transaction -> Dat -> Dat
addTrans t d = Map.insert (chksum t) t d

createTransaction :: String -> String -> Double -> String -> String -> Transaction
createTransaction fAddr tAddr val msg ts= 
    let
      csum = hash $ C8.pack $ concat $ [fAddr,tAddr,show val,msg,ts] :: Hash
    in
      T { fromAddr = fAddr, toAddr = tAddr, value = val, message = msg, tstamp = ts, chksum = show csum}
