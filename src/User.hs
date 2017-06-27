{-# LANGUAGE DeriveGeneric #-}

module User where

import GHC.Generics
import Data.Aeson

data User = User {
    uname        :: !String   ,
    password     :: !String   , 
    memberGroups :: ![String] ,
    friendsList  :: ![String] ,
    blockstamp   :: !String
}
  deriving (Generic,Show,Eq)


instance ToJSON User 
instance FromJSON User

data UserResponse = UserResponse {
    username :: !String   ,
    groups   :: ![String] ,
    flist    :: ![String] ,
    bstamp   :: !String
}
  deriving (Generic,Show,Eq)

instance ToJSON UserResponse 
instance FromJSON UserResponse
