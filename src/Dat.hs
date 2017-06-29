{-# LANGUAGE DeriveGeneric #-}
module Dat where

import Data.Aeson
import Data.Bson.Generic
import Data.Typeable
import GHC.Generics
import Crypto.Hash as Cryp
import qualified Data.ByteString.Char8 as C8

type Hash = Digest SHA256
type Dat = [Record]

data Record = T Transaction | GR GroupRegister | UR UserRegister | AF AddFriend
 deriving (Generic,Show,Eq)

instance ToJSON Record 
instance FromJSON Record

instance ToBSON Record
instance FromBSON Record

data ValueByUser = ValueByUser { person :: String, amount :: Double }
  deriving (Generic, Typeable, Show,Eq)

instance ToJSON ValueByUser 
instance FromJSON ValueByUser

instance ToBSON ValueByUser
instance FromBSON ValueByUser

data Transaction = Transaction {
    fromUser :: !String ,
    group    :: !String ,
    value    :: ![ValueByUser],
    message  :: !String ,
    tstamp   :: !String ,
    chksum   :: !String
}
 deriving (Generic,Show,Eq)

instance ToJSON Transaction 
instance FromJSON Transaction

instance ToBSON Transaction
instance FromBSON Transaction

data TransactionReg = TransactionReg {
  usr :: !String,
  grp :: !String,
  vals :: ![ValueByUser],
  msg :: !String,
  ts  :: !String
}
  deriving (Generic,Show,Eq)

instance ToJSON TransactionReg
instance FromJSON TransactionReg

instance ToBSON TransactionReg
instance FromBSON TransactionReg

data GroupRegister = GroupRegister {
     users       :: ![String],
     identifier :: String ,
     gname  :: !String ,
     description :: !String 
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

data AddFriend = AddFriend {
     user_id :: !String ,
     friend_id :: !String 
    }
 deriving (Generic,Show,Eq)

instance ToJSON AddFriend 
instance FromJSON AddFriend

instance ToBSON AddFriend
instance FromBSON AddFriend

newDat :: Dat
newDat = []

addRecord :: Record -> Dat -> Dat
addRecord t = (:) t 

numTransactions :: Dat -> Int
numTransactions = length 

registerUser :: String -> String -> Record
registerUser username password = UR $ UserRegister { name = username , pw = password }

createTransaction :: String -> String -> [ValueByUser] -> String -> String -> Record
createTransaction fAddr g val msg ts=
    let
      csum = ((hash . C8.pack) . concat) [fAddr,show val,msg,ts] :: Hash
    in
      T $ Transaction { fromUser = fAddr, group = g, value = val, message = msg, tstamp = ts, chksum = show csum}
