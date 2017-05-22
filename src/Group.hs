module Group where

data Group = G {
    id :: String ,
    users :: [String] ,
    transactions :: [String] ,
    blockstamp :: String
}