{-# LANGUAGE OverloadedStrings #-}

module BlockChain ( runQuery
    , insertBlock
    , getBlockByIndex
    , getBlockByHash
    , getNrBlocks
    , getBlocks
    , mkCache
    , fetchUser
    , fetchGroup
    , userLogin
    , registerGroup
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

getGroup :: Queue G.Group -> String -> Maybe G.Group
getGroup (Queue [] _) _ = Nothing
getGroup (Queue l _) gr = L.find (\x -> if (G.ident x) == gr then True else False) l

pop :: Queue a -> (Maybe a,Queue a)
pop (Queue [] size) = (Nothing, Queue [] size)
pop (Queue (x:xs) size) = (Just x,Queue xs size)

push :: Queue a -> a -> Queue a
push (Queue l size) x
  | (length l)+1 > size = Queue (x:(L.init l)) size
  | otherwise = Queue (x:l) size

data Cache = Cache {
  usersCache :: !(Queue User) ,
  groupsCache :: !(Queue G.Group) ,
  blockBucket :: BlockBuilder
}

mkCache :: Int -> Int -> Block -> Cache
mkCache x s b = Cache { usersCache = mkQueue x, groupsCache = mkQueue x, blockBucket = newBB b s}

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

--- fetchUser && userLogin aux funcs ---
update_cache_g found_group uc gc bb cache = atomically $ do
  let new_gc = push gc found_group
  let newCache = Cache { usersCache = uc, groupsCache = new_gc , blockBucket = bb}
  writeTVar cache newCache
update_cache found_user@(User {}) uc gc bb cache = atomically $ do
  let new_uc = push uc found_user
  let newCache = Cache { usersCache = new_uc , groupsCache = gc, blockBucket = bb }
  writeTVar cache newCache
checkRegG gr (GR (GroupRegister {identifier = x})) = if x==gr then True else False
checkRegG _ _ = False
checkReg usr (UR (UserRegister {name = x})) = if x==usr then True else False
checkReg _ _ = False
checkUserReg gr (UGR (UserGroupRegister {groupId = x})) = if x==gr then True else False
checkUserReg _ _ = False
checkGroupReg usr (UGR (UserGroupRegister {userId = x})) = if x==usr then True else False
checkGroupReg _ _ = False
filterUsersIds (UGR (UserGroupRegister {userId = x})) = Just x
filterUsersIds _ = Nothing
filterGroupIds (UGR (UserGroupRegister {groupId = x})) = Just x
filterGroupIds _ = Nothing
--- //// ---
userLogin :: TVar Cache -> Pipe -> String -> String -> IO Bool
userLogin cache pipe usr p = do 
    (Cache { usersCache = uc , groupsCache = gc, blockBucket = bb }) <- readTVarIO cache
    maybe (checkDatabase uc gc bb) encodeResp (getUser uc usr)
  where
    checkDatabase uc gc bb = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let userReg = L.find (checkReg usr) dats
      case userReg of
        Just (UR (UserRegister { pw = pwd })) -> do
          let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
          cur_block_nr <- (1-) `fmap` runQuery pipe getNrBlocks
          let found_user = User { uname = usr , password = pwd , memberGroups = gs , blockstamp = show cur_block_nr }
          update_cache found_user uc gc bb cache
          encodeResp found_user
        _ -> return False
    encodeResp (User { uname = u , password = pw }) = return (u == usr && pw == p)

fetchUser :: TVar Cache -> Pipe -> String -> IO (Maybe UserResponse)
fetchUser cache pipe usr = do 
    (Cache { usersCache = uc , groupsCache = gc , blockBucket = bb}) <- readTVarIO cache
    maybe (checkDatabase uc gc bb) encodeResp (getUser uc usr)
  where
    checkDatabase uc gc bb = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let userReg = L.find (checkReg usr) dats
      case userReg of
        Just (UR (UserRegister { pw = pwd })) -> do
          let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
          cur_block_nr <- (1-) `fmap` runQuery pipe getNrBlocks
          let found_user = User { uname = usr , password = pwd , memberGroups = gs , blockstamp = show cur_block_nr }
          update_cache found_user uc gc bb cache
          encodeResp found_user
        _ -> return Nothing
    encodeResp (User { uname = u , memberGroups = g }) = return (Just $ UserResponse { username = u , groups = g })

fetchGroup :: TVar Cache -> Pipe -> String -> IO (Maybe G.Group)
fetchGroup cache pipe gr = do 
    (Cache { usersCache = uc , groupsCache = gc , blockBucket = bb}) <- readTVarIO cache
    maybe (checkDatabase uc gc bb) encodeResp (getGroup gc gr)
  where
    checkDatabase uc gc bb = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let groupReg = L.find (checkRegG gr) dats
      case groupReg of
        Just (GR (GroupRegister { identifier = gname, description = descrip })) -> do
          let usrs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkUserReg gr) dats
          let trans = [] 
          cur_block_nr <- (1-) `fmap` runQuery pipe getNrBlocks
          let found_group = G.G { G.ident = gname , G.users = usrs, G.transactions = trans, G.desc = descrip, G.bstamp = show cur_block_nr }
          update_cache_g found_group uc gc bb cache
          encodeResp found_group
        _ -> return Nothing
    encodeResp g = return (Just g)

registerGroup :: TVar Cache -> Pipe -> GroupRegister -> IO Bool
registerGroup cache pipe gr = do
  g <- fetchGroup cache pipe (identifier gr)
  case g of
    Just x -> return False
    Nothing -> do
      Cache { usersCache = uc, groupsCache = gc, blockBucket = b } <- readTVarIO cache
      either 
        (\ bb -> atomically $ writeTVar cache (Cache { usersCache = uc, groupsCache = gc, blockBucket = bb}) ) 
        (\newBlock -> do
           _ <- runQuery pipe (insertBlock newBlock)
           let new_bb = newBB newBlock (size b)
           atomically $ writeTVar cache (Cache { usersCache = uc, groupsCache = gc, blockBucket = new_bb }) 
        ) 
        (addRec (GR gr) b)
      return True