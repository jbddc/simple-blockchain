{-# LANGUAGE DeriveGeneric #-}
module Dat where

import qualified Data.Map.Strict as Map
import Time.Types
import Data.Aeson
import Data.Bson.Generic
import GHC.Generics
import Crypto.Hash as Cryp
import qualified Data.ByteString.Char8 as C8

type Hash = Digest SHA256
type Dat = [Record]

data Record = T Transaction | GR GroupRegister | UR UserRegister | UGR UserGroupRegister
 deriving (Generic,Show,Eq)

instance ToJSON Record 
instance FromJSON Record

instance ToBSON Record
instance FromBSON Record

data Transaction = Transaction {
    fromAddr :: !String ,
    toAddr   :: !String ,
    value    :: !Double ,
    message  :: !String ,
    tstamp   :: !String ,
    chksum   :: !String
}
 deriving (Generic,Show,Eq)

instance ToJSON Transaction 
instance FromJSON Transaction

instance ToBSON Transaction
instance FromBSON Transaction

data GroupRegister = GroupRegister {
     testum :: !String 
    }
 deriving (Generic,Show,Eq)

instance ToJSON GroupRegister 
instance FromJSON GroupRegister

instance ToBSON GroupRegister
instance FromBSON GroupRegister

data UserRegister = UserRegister {
     name :: !String ,
     pw :: !String 
    }
 deriving (Generic,Show,Eq)

instance ToJSON UserRegister 
instance FromJSON UserRegister

instance ToBSON UserRegister
instance FromBSON UserRegister

data UserGroupRegister = UserGroupRegister {
    groupId :: !String ,
    userId  :: !String
}
 deriving (Generic,Show,Eq)

instance ToJSON UserGroupRegister 
instance FromJSON UserGroupRegister

instance ToBSON UserGroupRegister
instance FromBSON UserGroupRegister

newDat :: Dat
newDat = []

addRecord :: Record -> Dat -> Dat
addRecord t = (:) t 

numTransactions :: Dat -> Int
numTransactions = length 

createTransaction :: String -> String -> Double -> String -> String -> Transaction
createTransaction fAddr tAddr val msg ts=
    let
      csum = ((hash . C8.pack) . concat) [fAddr,tAddr,show val,msg,ts] :: Hash
    in
      Transaction { fromAddr = fAddr, toAddr = tAddr, value = val, message = msg, tstamp = ts, chksum = show csum}
