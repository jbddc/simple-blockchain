{-# LANGUAGE OverloadedStrings #-}

module BlockChain ( runQuery
    , insertBlock
    , getBlockByIndex
    , mkQueue
    , push
    , peek
    , pop
    , getBlock
    , getNrBlocks
) where

import Block
import Dat
import Data.Maybe
import qualified Data.List as L
import Database.MongoDB
import Data.Bson.Generic

---- Simple Queue implementation ----

data Queue a = Queue [a] Int deriving (Show, Eq)

mkQueue :: Int -> Queue a
mkQueue x = if x>0 then Queue [] x else Queue [] 1

peek :: Queue a -> Maybe a
peek (Queue [] _) = Nothing
peek (Queue l _) = return $ head l

pop :: Queue a -> (Maybe a,Queue a)
pop (Queue [] size) = (Nothing, Queue [] size)
pop (Queue (x:xs) size) = (Just x,Queue xs size)

push :: Queue a -> a -> Queue a
push (Queue l size) x
  | (length l)+1 > size = Queue (x:(L.init l)) size
  | otherwise = Queue (x:l) size

---- /Simple Queue implementation ----

dbName = "bsw_blockchain"

runQuery pipe action = access pipe master dbName action

insertBlock :: Block ->  Action IO Value
insertBlock b = insert "blocks" (toBSON b)

getBlockByIndex :: Int -> Action IO (Maybe Block)
getBlockByIndex index = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["index" =: index] "blocks")
    
getNrBlocks :: Action IO Int
getNrBlocks = count (select []  "blocks")

getBlock :: String -> Action IO (Maybe Block)
getBlock blockHash = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["blockHash" =: blockHash] "blocks")


