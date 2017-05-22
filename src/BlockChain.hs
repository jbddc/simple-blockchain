{-# LANGUAGE OverloadedStrings #-}

module BlockChain ( runQuery
    , insertBlock
    , getBlockByIndex
    , getBlockByHash
    , getNrBlocks
    , mkCache
    , fetchUser
) where

import Block
import Dat
import User
import qualified Group as G
import Data.Maybe
import Data.SecureMem
import qualified Data.List as L (find)
import Control.Concurrent.STM
import qualified Data.List as L
import Database.MongoDB
import Data.Bson.Generic

---- Simple Caching implementation ----

data Queue a = Queue [a] Int deriving (Show, Eq)

mkQueue :: Int -> Queue a
mkQueue x = if x>0 then Queue [] x else Queue [] 1

peek :: Queue a -> Maybe a
peek (Queue [] _) = Nothing
peek (Queue l _) = return $ head l

-- TODO
getUser :: Queue User -> String -> Maybe User
getUser (Queue [] _) _ = Nothing
getUser (Queue l _) usr = L.find (\x -> if (uname x) == usr then True else False) l

pop :: Queue a -> (Maybe a,Queue a)
pop (Queue [] size) = (Nothing, Queue [] size)
pop (Queue (x:xs) size) = (Just x,Queue xs size)

push :: Queue a -> a -> Queue a
push (Queue l size) x
  | (length l)+1 > size = Queue (x:(L.init l)) size
  | otherwise = Queue (x:l) size

data Cache = Cache {
  usersCache :: !(Queue User) ,
  groupsCache :: !(Queue G.Group)
}

mkCache :: Int -> Cache
mkCache x = Cache { usersCache = mkQueue x, groupsCache = mkQueue x}

---- /Simple Queue implementation ----

dbName = "bsw_blockchain"

runQuery pipe action = access pipe master dbName action

insertBlock :: Block ->  Action IO Value
insertBlock b = insert "blocks" (toBSON b)

getBlockByIndex :: Int -> Action IO (Maybe Block)
getBlockByIndex index = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["index" =: index] "blocks")
    
getNrBlocks :: Action IO Int
getNrBlocks = count (select []  "blocks")

getBlockByHash :: String -> Action IO (Maybe Block)
getBlockByHash blockHash = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["blockHash" =: blockHash] "blocks")

getBlocks :: Action IO [Block]
getBlocks = do
  result <- liftDB $ find (select [] "blocks")
  let rresult = rest result
  (\y -> map (\x -> fromJust $ (fromBSON x :: Maybe Block)) y) `fmap` rresult

fetchUser :: TVar Cache -> Pipe -> String -> IO (Maybe UserResponse)
fetchUser cache pipe usr = do 
    (Cache { usersCache = uc , groupsCache = gc }) <- readTVarIO cache
    maybe (checkDatabase uc gc) encodeResp (getUser uc usr)
  where
    update_cache found_user uc gc = atomically $ do
      let new_uc = push uc found_user
      let newCache = Cache { usersCache = new_uc , groupsCache = gc }
      writeTVar cache newCache
    
    checkReg usr (UR (UserRegister {name = x})) = if x==usr then True else False
    checkReg _ _ = False
    checkGroupReg usr (UGR (UserGroupRegister {userId = x})) = if x==usr then True else False
    checkGroupReg _ _ = False
    filterGroupIds (UGR (UserGroupRegister {groupId = x})) = Just x
    filterGroupIds _ = Nothing
    
    checkDatabase uc gc = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let userReg = L.find (checkReg usr) dats
      case userReg of
        Just (UR (UserRegister { pw = pwd })) -> do
          let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
          let cur_block_nr = undefined
          let found_user = User { uname = usr , password = pwd , memberGroups = gs , blockstamp = cur_block_nr }
          update_cache found_user uc gc
          encodeResp found_user
        _ -> return Nothing
    
    encodeResp (User { uname = u , memberGroups = g }) = return (Just $ UserResponse { username = u , groups = g })