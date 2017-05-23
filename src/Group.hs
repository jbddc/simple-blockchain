{-# LANGUAGE DeriveGeneric #-}

module Group where

import GHC.Generics
import Data.Aeson

data Group = G {
    ident :: String ,
    users :: [String] ,
    transactions :: [String] ,
    desc :: String ,
    bstamp :: String
}
  deriving (Generic,Show,Eq)

instance ToJSON Group 
instance FromJSON Group