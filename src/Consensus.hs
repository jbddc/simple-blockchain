module Consensus where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Concurrent as Concurrent
import Time.System
import System.Random
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

group :: PrivateGroup
group = fromJust $ makeGroup "consensus"

sendBlock :: Connection -> Block.Block -> IO ()
sendBlock conn block = do
  let msg = Outgoing {outOrdering = Fifo, outDiscard = False, outData = BSL.toStrict $ encode block, outGroups = [Consensus.group], outMsgType = 1}
  send msg conn

listenNetworkBlocks :: Mongo.Pipe -> (Chan R Message, Connection) -> IO ()
listenNetworkBlocks pipe (chan,conn) = do
  result <- startReceive conn
  msg <- readChan chan
  case msg of
    Just (Regular inMsg) -> do
      case inMsgType inMsg of
        1 -> do
          let mBlock = decode $ BSL.fromStrict $ inData inMsg
          maybe (return ()) (processNewBlock pipe) mBlock
        2 -> randomFalseTrue >>= (\a -> if not a then return () else do
          let blockIndex = read $ B.unpack $ inData inMsg
          mBlock <- runQuery pipe (getBlockByIndex blockIndex)
          maybe (return ()) (sendBlock conn) mBlock)
        _ -> putStrLn "TODO"
    Nothing -> putStrLn "Bad message..."
    _ -> putStrLn "TODO"  
  listenNetworkBlocks pipe (chan,conn)

randomFalseTrue :: IO Bool
randomFalseTrue = do
  percent <- randomRIO (0,100) :: IO Int
  return (percent > 80)

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
startConsensus :: Mongo.Pipe -> IO ()
startConsensus pipe = do
  n <- Consensus.name
  let config = Conf { address = Just "localhost" , port = Nothing, desiredName = n, priority = False, groupMembership = True, authMethods = [] }
  (chan,conn) <- connect config
  join Consensus.group conn
  --listener <- Concurrent.forkIO $ listenNetworkBlocks
  listenNetworkBlocks pipe (chan,conn)
  disconnect conn

treatBlock :: Block.Block -> IO ()
treatBlock = undefined

-- tratar o caso em que não há resposta, aka, não há nodos na blockchain
consensusHandshake :: Mongo.Pipe -> IO (Maybe Block.Block)
consensusHandshake pipe = do
    -- criar nome temporario
    tempName <- Consensus.name
    -- obter o bloco mais recente (WARNING: posso nao ter blocos)
    currentIndex <- (1-) `fmap` runQuery pipe getNrBlocks
    -- establish connection
    let config = Conf { address = Nothing , port = Nothing, desiredName = tempName, priority = False, groupMembership = True, authMethods = [] }
    (chan,conn) <- connect config
    -- juntar ao grupo "consensus"
    join Consensus.group conn
  
  
    -- obter index do block mais recente actual (WARNING: pode nao haver nodos na rede)
    let indReqMsg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = B.pack "", outGroups = [Consensus.group], outMsgType = 3}
    send indReqMsg conn
    startReceive conn
    (blockMap,index) <- recvIndex Map.empty chan

    disconnect conn

   -- ir buscar blocos entre aquele que eu tenho na BD e o bloco mais recente da rede

    -- tratar queue de blocos recebidos entretanto
    mapM_ treatBlock $ elems blockMap

    -- retornar ultimo bloco actual para dar startup à cache
    runQuery pipe getLastBlock 
    
recvIndex :: (Map.Map Integer Block.Block) -> (Chan R Message) -> IO (Map.Map Integer Block.Block,Int)
recvIndex blockMap chan = do
     m <- readChan chan  
     case m of
       Just (Regular msg) -> case inMsgType msg of
         1 -> 
           maybe 
            (recvIndex blockMap chan) 
            (\bl -> recvIndex (insert (Block.index bl) bl blockMap) chan) 
            (decode $ BSL.fromStrict $ inData msg :: Maybe Block.Block)
         3 -> return (blockMap, read $ B.unpack $ inData msg)
         _ -> recvIndex blockMap chan
       _ -> recvIndex blockMap chan
