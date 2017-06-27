module Consensus where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Concurrent as Concurrent
import Time.System
import System.Random
import System.Exit
import qualified Control.Monad as Monad
import Crypto.Hash
import Control.Concurrent.STM
import Data.Aeson
import Data.Maybe
import qualified Database.MongoDB as Mongo
import Spread.Client
import Control.Concurrent.Chan.Closeable
import Dat
import Data.Map.Strict as Map
import qualified Block as Block
import BlockChain

type Bucket = TVar Block.BlockBuilder

data BlockParseResult = HashMismatch | IndexMismatch | DatabaseError | OK

spread_address = Just "alcetipe.dyndns.org"
--spread_address = Just "localhost"
spread_port = Just 4803

group :: PrivateGroup
group = fromJust $ makeGroup "consensus"

sendBlock :: Connection -> Block.Block -> IO ()
sendBlock conn block = do
  let msg = Outgoing {outOrdering = Fifo, outDiscard = False, outData = BSL.toStrict $ encode block, outGroups = [Consensus.group], outMsgType = 1}
  send msg conn

listenNetworkBlocks :: Mongo.Pipe -> TVar Cache -> (Chan R Message, Connection) -> IO ()
listenNetworkBlocks pipe cache (chan,conn) = do
  isC <- startReceive conn
  case isC of
     True -> stopReceive conn >>= const (putStrLn "Connection to Spread daemon lost! Please quit this node and restart connection...")
     False -> return ()
  msg <- readChan chan
  case msg of
    Just (Regular inMsg) -> do
      case inMsgType inMsg of
        -- new block arrives
        1 -> do
          let mBlock = decode $ BSL.fromStrict $ inData inMsg
          maybe (return ()) (processNewBlock pipe) mBlock
        -- request for block by index
        2 -> do
          let blockIndex = read $ B.unpack $ inData inMsg
          putStr "Received Request for Block: " 
          putStrLn $ B.unpack $ inData inMsg
          mBlock <- runQuery pipe (getBlockByIndex blockIndex)
          maybe (return () >> putStr "Couldn't send block") (\blc -> sendBlock conn blc >> putStrLn "Block Sent.") mBlock
        -- request for current block index
        3 -> do
          response <- runQuery pipe getNrBlocks
          let respMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack $ show (response -1), outGroups = [Consensus.group], outMsgType = 4}
          send respMsg conn
        typ -> putStrLn $ "TODO Msg Type: "++(show typ)
    Just (Membership memMsg) -> putStrLn $ show $ numMembers memMsg
    Nothing -> putStrLn "Lost connection to Spread Daemon, press [enter] to acknowledge this message and quit..." >> disconnect conn >>= const exitFailure
    _ -> putStrLn "TODO"  
  listenNetworkBlocks pipe cache (chan,conn)

randomFalseTrue :: IO Bool
randomFalseTrue = do
  percent <- randomRIO (0,100) :: IO Int
  return (percent > 60)

processNewBlock :: Mongo.Pipe -> Block.Block -> IO ()
processNewBlock pipe block = do
  _ <- runQuery pipe getLastBlock
  print "TODO"

processRecord :: Connection -> Record -> Bucket -> IO ()
processRecord conn rec bucket = do
  _bucket <- readTVarIO bucket
  either 
    (\bb -> atomically $ writeTVar bucket bb)
    (sendBlock conn)
    (Block.addRec rec _bucket)

name :: IO PrivateName
name = do
  time <- timeCurrent
  rand <- randomIO :: IO Int
  let preH = init $ show time ++ show rand
  let h = hash $ B.pack preH :: Digest SHA256
  return . mkPrivateName . B.pack . show $ h

-- WARNING: IR BUSCAR BLOCOS ATÉ AO MAIS RECENTE
startConsensus :: (Chan R Message,Connection) -> Mongo.Pipe -> TVar Cache -> IO ()
startConsensus (chan,conn) pipe cache = do
  join Consensus.group conn
  startReceive conn
  (numMembers,blocksMap) <- getNumMembers Map.empty chan
  if numMembers <= 1
    then do
      mapM_ (\b -> parseInsertBlock pipe b (fromIntegral $ Block.index b)) $ elems blocksMap
      _cache <- readTVarIO cache
      let old_bb = blockBucket _cache
      last_block <- runQuery pipe getLastBlock
      maybe (return ()) (\last_b -> atomically $ writeTVar cache (_cache { blockBucket = old_bb { Block.prevBlock = last_b } })) last_block
      listenNetworkBlocks pipe cache (chan,conn)
      disconnect conn
    else do
      currentIndex <- ((flip (-)) 1) `fmap` runQuery pipe getNrBlocks
      let indReqMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack "", outGroups = [Consensus.group], outMsgType = 3}
      send indReqMsg conn
      (index,blocksMap') <- recvIndex blocksMap chan
      case index > currentIndex of
        True -> do
          blocksMap'' <- syncFromTo blocksMap' pipe (currentIndex+1) index (chan,conn) 
          mapM_ (\b -> parseInsertBlock pipe b (fromIntegral $ Block.index b)) $ elems blocksMap''
          _cache <- readTVarIO cache
          let old_bb = blockBucket _cache
          last_block <- runQuery pipe getLastBlock
          maybe (return ()) (\last_b -> atomically $ writeTVar cache (_cache { blockBucket = old_bb { Block.prevBlock = last_b } })) last_block
          listenNetworkBlocks pipe cache (chan,conn)
          disconnect conn
        False -> do
          let blocksMap'' = blocksMap
          mapM_ (\b -> parseInsertBlock pipe b (fromIntegral $ Block.index b)) $ elems blocksMap''
          _cache <- readTVarIO cache
          let old_bb = blockBucket _cache
          last_block <- runQuery pipe getLastBlock
          maybe (return ()) (\last_b -> atomically $ writeTVar cache (_cache { blockBucket = old_bb { Block.prevBlock = last_b } })) last_block
          listenNetworkBlocks pipe cache (chan,conn)
          disconnect conn

consensusHandshake :: Mongo.Pipe -> IO (Maybe Block.Block)
consensusHandshake pipe = do
    -- criar nome temporario
    tempName <- Consensus.name
    putStr  "Name created: " >> print tempName
    -- obter o bloco mais recente 
    currentIndex <- ((flip (-)) 1) `fmap` runQuery pipe getNrBlocks
    putStr  "Current Index: " >> print currentIndex
    -- establish connection
    let config = Conf { address = spread_address , port = spread_port, desiredName = tempName, priority = False, groupMembership = True, authMethods = [] }
    (chan,conn) <- connect config
    join Consensus.group conn
    startReceive conn
    (numMembers,_) <- getNumMembers Map.empty chan 
    putStr  "numMembers: " >> print numMembers
    if numMembers <= 1 
      then do
        disconnect conn
        putStrLn "Leaving..."
        -- retornar ultimo bloco actual para dar startup à cache
        runQuery pipe getLastBlock
      else do
        putStrLn "Syncing..."
        let indReqMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack "", outGroups = [Consensus.group], outMsgType = 3}
        send indReqMsg conn
        (index,_) <- recvIndex Map.empty chan
        putStr "Index: " >> print index
        -- ir buscar blocos entre aquele que eu tenho na BD e o bloco mais recente da rede
        case index > currentIndex of
          True -> do
            syncFromTo Map.empty pipe (currentIndex+1) index (chan,conn)
            disconnect conn
            runQuery pipe getLastBlock
          _ -> do 
            disconnect conn
            runQuery pipe getLastBlock

getNumMembers :: Map.Map Integer Block.Block -> Chan R Message -> IO (Int,Map.Map Integer Block.Block)
getNumMembers blocksMap chan = do
  m <- readChan chan
  case m of
    Just (Membership memMsg) -> return $ (numMembers memMsg,blocksMap)
    Just (Regular msg) -> case inMsgType msg of
         1 -> 
           maybe 
            (getNumMembers blocksMap chan) 
            (\bl -> getNumMembers (insert (Block.index bl) bl blocksMap) chan) 
            (decode $ BSL.fromStrict $ inData msg :: Maybe Block.Block)
         _ -> getNumMembers blocksMap chan
    _ -> getNumMembers blocksMap chan

recvIndex :: Map.Map Integer Block.Block -> (Chan R Message) -> IO (Int,Map.Map Integer Block.Block)
recvIndex blocksMap chan = do
     m <- readChan chan  
     case m of
       Just (Regular msg) -> case inMsgType msg of
         1 -> 
           maybe 
            (recvIndex blocksMap chan) 
            (\bl -> recvIndex (insert (Block.index bl) bl blocksMap) chan) 
            (decode $ BSL.fromStrict $ inData msg :: Maybe Block.Block)
         4 -> return $ (read $ B.unpack $ inData msg,blocksMap)
         _ -> recvIndex blocksMap chan
       _ -> recvIndex blocksMap chan

syncFromTo :: Map.Map Integer Block.Block -> Mongo.Pipe -> Int -> Int -> (Chan R Message,Connection) -> IO (Map.Map Integer Block.Block)
syncFromTo blocksMap pipe from to (chan,conn) = do
    let firstMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack $ show from, outGroups = [Consensus.group], outMsgType = 2}
    putStrLn $ "Sending request for block #"++(show from)
    send firstMsg conn
    auxSyncFromTo blocksMap pipe from to (chan,conn)

auxSyncFromTo :: Map.Map Integer Block.Block -> Mongo.Pipe -> Int -> Int -> (Chan R Message,Connection) -> IO (Map.Map Integer Block.Block)
auxSyncFromTo blocksMap pipe from to (chan,conn) 
  | from <= to = do
      m <- readChan chan
      case m of
        Just (Regular msg) -> case inMsgType msg of
          1 ->
            maybe
            (do
              let retryMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack $ show from, outGroups = [Consensus.group], outMsgType = 2}
              putStrLn $ "Sending request for block #"++(show from)
              send retryMsg conn
              auxSyncFromTo blocksMap pipe from to (chan,conn)
            )
            (\blck -> parseInsertBlock pipe blck from >>= (\x -> case x of
              IndexMismatch -> auxSyncFromTo (Map.insert (Block.index blck) blck blocksMap) pipe from to (chan,conn)
              HashMismatch -> do
                let retryMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack $ show from, outGroups = [Consensus.group], outMsgType = 2}
                putStrLn $ "Sending request for block #"++(show from)
                send retryMsg conn
                auxSyncFromTo (Map.insert (Block.index blck) blck blocksMap) pipe from to (chan,conn)
              DatabaseError -> error "Database Error!"
              OK -> do
                if from==to then return blocksMap else do
                  let nextMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack $ show (from+1), outGroups = [Consensus.group], outMsgType = 2}
                  putStrLn $ "Sending request for block #"++(show $ from+1)
                  send nextMsg conn
                  auxSyncFromTo blocksMap pipe (from+1) to (chan,conn))
            )
            (decode $ BSL.fromStrict $ inData msg :: Maybe Block.Block)
          _ -> auxSyncFromTo blocksMap pipe from to (chan,conn)
        _ -> auxSyncFromTo blocksMap pipe from to (chan,conn)
  | otherwise = return blocksMap

parseInsertBlock :: Mongo.Pipe -> Block.Block -> Int -> IO BlockParseResult
parseInsertBlock pipe blck 0 = do
  if Block.index blck /= 0 then return IndexMismatch else do
  runQuery pipe (insertBlock blck)
  return OK
parseInsertBlock pipe blck indx = do
  if Block.index blck /= (fromIntegral indx) then return IndexMismatch else do
    mBlock <- runQuery pipe getLastBlock
    maybe
      (return DatabaseError)
      (\prev_block -> do
        if (Block.blockHash prev_block) /= (Block.prevHash blck) then return HashMismatch else do
          runQuery pipe (insertBlock blck)
          return OK
      )
      mBlock
      