{-# LANGUAGE OverloadedStrings #-}

module BlockChain ( runQuery
    , insertBlock
    , getBlockByIndex
    , getBlockByHash
    , getNrBlocks
    , getBlocks
    , getBlocksFrom
    , getLastBlock
    , Cache
    , attempted_blocks
    , blockBucket
    , spread_con
    , mkCache
    , fetchTransaction
    , fetchUser
    , fetchGroup
    , addFriend
    , userLogin
    , registerGroup
    , regTransaction
    , regUser
) where

import Block
import Dat
import User
import qualified Group as G
import Data.Maybe
import Data.SecureMem
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L (find)
import Control.Concurrent.STM
import qualified Data.List as L
import System.Random
import qualified Spread.Client as Spread
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

getTrans :: Queue Transaction -> String -> Maybe Transaction
getTrans (Queue [] _) _ = Nothing
getTrans (Queue l _) tr = L.find (\x -> if (chksum x) == tr then True else False) l

pop :: Queue a -> (Maybe a,Queue a)
pop (Queue [] size) = (Nothing, Queue [] size)
pop (Queue (x:xs) size) = (Just x,Queue xs size)

pushReplaceG :: Queue G.Group -> G.Group -> Queue G.Group
pushReplaceG q@(Queue l size) g = let l' = filter ((/=) (G.ident g) . G.ident) l in Queue (g:l') size

pushReplace :: Queue User -> User -> Queue User
pushReplace q@(Queue l size) usr = let l' = filter ((/=) (uname usr) . uname) l in Queue (usr:l') size

push :: Queue a -> a -> Queue a
push (Queue l size) x
  | (length l)+1 > size = Queue (x:(L.init l)) size
  | otherwise = Queue (x:l) size

data Cache = Cache {
  spread_con :: !(Spread.Connection),
  usersCache :: !(Queue User) ,
  groupsCache :: !(Queue G.Group) ,
  transCache :: !(Queue Transaction),
  blockBucket :: BlockBuilder,
  attempted_blocks :: ![Block]
}

mkCache :: Spread.Connection -> Int -> Int -> Block -> Cache
mkCache sconn x s b = Cache { spread_con = sconn, usersCache = mkQueue x, groupsCache = mkQueue x, transCache = mkQueue x, blockBucket = newBB b s, attempted_blocks = []}

---- /Simple Queue implementation ----

dbName = "bsw_blockchain"

runQuery pipe action = access pipe master dbName action

insertBlock :: Block ->  Action IO Value
insertBlock b = insert "blocks" (toBSON b)

getBlockByIndex :: Int -> Action IO (Maybe Block)
getBlockByIndex index = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["index" =: index] "blocks")
    
getNrBlocks :: Action IO Int
getNrBlocks = count (select []  "blocks")

getBlocksFrom :: Integer -> Action IO [Block]
getBlocksFrom from = do
  result <- liftDB $ find (select [] "blocks")
  let rresult = rest result
  (\y -> filter ((>= from) . Block.index) $ map (\x -> fromJust $ (fromBSON x :: Maybe Block)) y) `fmap` rresult

getBlockByHash :: String -> Action IO (Maybe Block)
getBlockByHash blockHash = (maybe Nothing (\x -> fromBSON x :: Maybe Block)) `fmap` findOne (select ["blockHash" =: blockHash] "blocks")

getLastBlock :: Action IO (Maybe Block)
getLastBlock = do
  nr <- getNrBlocks
  if nr==0 then return Nothing else getBlockByIndex (nr-1)

getBlocks :: Action IO [Block]
getBlocks = do
  result <- liftDB $ find (select [] "blocks")
  let rresult = rest result
  (\y -> map (\x -> fromJust $ (fromBSON x :: Maybe Block)) y) `fmap` rresult

--- fetchUser && userLogin aux funcs ---
update_cache_replace_g found_group cache = atomically $ do
  _cache <- readTVar cache
  let new_gc = pushReplaceG (groupsCache _cache) found_group
  let newCache = _cache { groupsCache = new_gc }
  writeTVar cache newCache
update_cache_replace found_user cache = atomically $ do
  _cache <- readTVar cache
  let new_uc = pushReplace (usersCache _cache) found_user
  let newCache = _cache { usersCache = new_uc }
  writeTVar cache newCache
update_cache_t found_trans cache = atomically $ do
  _cache <- readTVar cache
  let new_tc = push (transCache _cache) found_trans
  let newCache =  _cache { transCache = new_tc }
  writeTVar cache newCache
update_cache_g found_group cache = atomically $ do
  _cache <- readTVar cache
  let new_gc = push (groupsCache _cache) found_group
  let newCache =  _cache { groupsCache = new_gc }
  writeTVar cache newCache
update_cache found_user@(User {}) cache = atomically $ do
  _cache <- readTVar cache
  let new_uc = push (usersCache _cache) found_user
  let newCache = _cache { usersCache = new_uc }
  writeTVar cache newCache
checkTrans g (T (Transaction { Dat.group = _g , Dat.chksum = ck })) = if g==_g then ck else ""
checkTrans _ _ = ""
checkTrans2 tr (T (Transaction {Dat.chksum = ck})) = if ck==tr then True else False
checkTrans2 _ _ = False
checkRegG gr (GR (GroupRegister {identifier = x})) = if x==gr then True else False
checkRegG _ _ = False
checkReg usr (UR (UserRegister {name = x})) = if x==usr then True else False
checkReg _ _ = False
checkGroupReg usr (GR (GroupRegister { users = usrs })) = if usr `elem` usrs then True else False
checkGroupReg _ _ = False
checkAddFriend usr (AF (AddFriend {user_id = uid , friend_id = fid })) = if usr==uid || usr==fid then True else False
checkAddFriend _ _ = False
filterAF usr (AF af) = if usr == (friend_id af) then (user_id af) else (friend_id af)
filterAF _ _ = "" 
filterGroupIds (GR (GroupRegister {identifier = x})) = Just x
filterGroupIds _ = Nothing
--- //// ---

-- GET

userLogin :: TVar Cache -> Pipe -> String -> String -> IO Bool
userLogin cache pipe usr p = do 
    _cache <- readTVarIO cache
    maybe (checkDatabase _cache) encodeResp (getUser (usersCache _cache) usr)
  where
    checkDatabase _cache = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let userReg = L.find (checkReg usr) dats
      case userReg of
        Just (UR (UserRegister { pw = pwd })) -> do
          let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
          cur_block_nr <- ((flip (-)) 1) `fmap` runQuery pipe getNrBlocks
          let fl = filter (/="") $ map (filterAF usr) $ filter (checkAddFriend usr) dats
          let found_user = User { uname = usr , password = pwd , memberGroups = gs , friendsList = fl , blockstamp = show cur_block_nr }
          update_cache found_user cache
          encodeResp found_user
        _ -> return False
    encodeResp (User { uname = u , password = pw }) = return (u == usr && pw == p)

fetchUser :: TVar Cache -> Pipe -> String -> IO (Maybe UserResponse)
fetchUser cache pipe usr = do 
    _cache <- readTVarIO cache
    maybe (checkDatabase) (updateCache pipe cache) (getUser (usersCache _cache) usr)
  where
    checkDatabase = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let userReg = L.find (checkReg usr) dats
      case userReg of
        Just (UR (UserRegister { pw = pwd })) -> do
          let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
          cur_block_nr <- ((flip (-)) 1) `fmap` runQuery pipe getNrBlocks
          let fl = filter (/="") $ map (filterAF usr) $ filter (checkAddFriend usr) dats
          let found_user = User { uname = usr , password = pwd , memberGroups = gs , friendsList = fl , blockstamp = show cur_block_nr }
          update_cache found_user cache
          encodeResp found_user
        _ -> return Nothing
    updateCache pipe cache justUsr = do
      let last_block_idx = read $ blockstamp justUsr
      db_last_block <- maybe (error "Database error!") id `fmap` runQuery pipe getLastBlock
      let db_last_block_idx = fromIntegral $ Block.index db_last_block
      if last_block_idx >= db_last_block_idx then encodeResp justUsr else do
        blocks <- runQuery pipe (getBlocksFrom (fromIntegral (last_block_idx+1)))
        let dats = concat $ map dat blocks
        let gs = map fromJust $ filter (/=Nothing) $ map filterGroupIds $ filter (checkGroupReg usr) dats
        let fl = filter (/="") $ map (filterAF usr) $ filter (checkAddFriend usr) dats
        let found_user = justUsr { memberGroups = (memberGroups justUsr)++gs , friendsList = (friendsList justUsr)++fl , blockstamp = show $ Block.index db_last_block}
        update_cache_replace found_user cache
        encodeResp found_user
    encodeResp (User { uname = u , memberGroups = g , blockstamp = stamp , friendsList = fl }) = return (Just $ UserResponse { username = u , groups = g , bstamp = stamp , flist = fl })

fetchTransaction :: TVar Cache -> Pipe -> String -> IO (Maybe Transaction)
fetchTransaction cache pipe tr = do 
    _cache <- readTVarIO cache
    maybe (checkDatabase _cache tr) (return . Just) (getTrans (transCache _cache) tr)
  where
    checkDatabase _cache tr = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let transReg = L.find (checkTrans2 tr) dats
      case transReg of
        Just (T t@(Transaction {})) -> do
          let found_trans = t 
          update_cache_t found_trans cache
          return (Just found_trans)
        _ -> return Nothing

fetchGroup :: TVar Cache -> Pipe -> String -> IO (Maybe G.Group)
fetchGroup cache pipe gr = do 
    _cache <- readTVarIO cache
    maybe (checkDatabase _cache gr) (updateCache pipe cache) (getGroup (groupsCache _cache) gr)
  where
    checkDatabase _cache gr = do
      blocks <- runQuery pipe getBlocks
      let dats = concat $ map dat blocks
      let groupReg = L.find (checkRegG gr) dats
      case groupReg of
        Just (GR (GroupRegister { gname = g_name , identifier = ide, description = descrip, users = usrs0 })) -> do
          let trans =  filter (/="") $ map (checkTrans ide) dats 
          cur_block_nr <- ((flip (-)) 1) `fmap` runQuery pipe getNrBlocks
          let found_group = G.G { G.ident = ide , G.name = g_name , G.users = usrs0, G.transactions = trans, G.desc = descrip, G.bstamp = show cur_block_nr }
          update_cache_g found_group cache
          encodeResp found_group
        _ -> return Nothing
    updateCache pipe cache justGroup = do
      let last_block_idx = read $ G.bstamp justGroup
      db_last_block <- maybe (error "Database error!") id `fmap` runQuery pipe getLastBlock
      let db_last_block_idx = fromIntegral $ Block.index db_last_block
      if last_block_idx >= db_last_block_idx then encodeResp justGroup else do
        blocks <- runQuery pipe (getBlocksFrom (fromIntegral (last_block_idx+1)))
        let dats = concat $ map dat blocks
        let trans =  filter (/="") $ map (checkTrans (G.ident justGroup)) dats 
        let found_group = justGroup { G.users = (G.users justGroup), G.transactions = trans++(G.transactions justGroup), G.bstamp = show $ Block.index db_last_block}
        update_cache_replace_g found_group cache
        encodeResp found_group
    encodeResp g = return (Just g)

-- POST
addFriend :: TVar Cache -> Pipe -> AddFriend -> IO Bool
addFriend cache pipe af = do
  usr <- fetchUser cache pipe (user_id af)
  case usr of
    Nothing -> return False
    Just (UserResponse { flist = fl }) -> do
      if (friend_id af) `elem` fl then return False else do
        friend <- fetchUser cache pipe (friend_id af)
        if isNothing friend then return False else do
          _cache <- readTVarIO cache
          either
            (\ bb -> atomically $ writeTVar cache (_cache { blockBucket = bb }))
            (\newBlock -> do
               sendBlock (spread_con _cache) newBlock
               let new_bb = newBB newBlock (size (blockBucket _cache))
               atomically $ do
                 _c <- readTVar cache
                 let _atb = attempted_blocks _c
                 writeTVar cache (_c { blockBucket = new_bb, attempted_blocks = newBlock:_atb }) 
            ) 
            (addRec (AF af) (blockBucket _cache))
          return True

registerGroup :: TVar Cache -> Pipe -> GroupRegister -> IO Bool
registerGroup cache pipe gr = do
  _cache <- readTVarIO cache
  either 
    (\ bb -> atomically $ writeTVar cache (_cache { blockBucket = bb}) ) 
    (\newBlock -> do
       sendBlock (spread_con _cache) newBlock
       let new_bb = newBB newBlock (size (blockBucket _cache))
       atomically $ do
         _c <- readTVar cache
         let _atb = attempted_blocks _c
         writeTVar cache (_c { blockBucket = new_bb, attempted_blocks = newBlock:_atb }) 
    ) 
    (addRec (GR gr) (blockBucket _cache))
  return True

regUser :: TVar Cache -> Pipe -> UserRegister -> IO Bool
regUser cache pipe ureg = do
  usr <- fetchUser cache pipe (name ureg)
  case usr of
    Just x -> return False
    Nothing -> do
      _cache <- readTVarIO cache
      either
        (\ bb -> atomically $ writeTVar cache (_cache { blockBucket = bb }))
        (\newBlock -> do
           sendBlock (spread_con _cache) newBlock
           let new_bb = newBB newBlock (size (blockBucket _cache))
           atomically $ do
             _c <- readTVar cache
             let _atb = attempted_blocks _c
             writeTVar cache (_c { blockBucket = new_bb, attempted_blocks = newBlock:_atb }) 
        )
        (addRec (UR ureg) (blockBucket _cache))
      return True

regTransaction :: TVar Cache -> Pipe -> TransactionReg -> IO Bool
regTransaction cache pipe tran = do
  _cache <- readTVarIO cache
  _usr <- fetchUser cache pipe (usr tran)
  _gr <- fetchGroup cache pipe (grp tran)
  case _usr of
    Nothing -> return False
    Just __usr -> case _gr of
      Nothing -> return False
      Just gr -> do 
        either 
          (\ bb -> atomically $ writeTVar cache (_cache { blockBucket = bb}) ) 
          (\newBlock -> do
             sendBlock (spread_con _cache) newBlock
             let new_bb = newBB newBlock (size (blockBucket _cache))
             atomically $ do
               _c <- readTVar cache
               let _atb = attempted_blocks _c
               writeTVar cache (_c { blockBucket = new_bb, attempted_blocks = newBlock:_atb }) 
          ) 
          (addRec (createTransaction (usr tran) (grp tran) (vals tran) (msg tran) (ts tran)) (blockBucket _cache))
        return True

sendBlock :: Spread.Connection -> Block.Block -> IO ()
sendBlock conn block = do
  let msg = Spread.Outgoing {Spread.outOrdering = Spread.Fifo, Spread.outDiscard = False, Spread.outData = BSL.toStrict $ encode block, Spread.outGroups = [_group], Spread.outMsgType = 1}
  Spread.send msg conn

_group :: Spread.PrivateGroup
_group = fromJust $ Spread.makeGroup "consensus"
