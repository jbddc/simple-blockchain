module Consensus where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Concurrent as Concurrent
import Time.System
import System.Random
import Crypto.Hash
import Control.Concurrent.STM
import Data.Aeson
import Data.Maybe
import Spread.Client
import Control.Concurrent.Chan.Closeable
import Dat
import Block
import BlockChain

type Bucket = TVar BlockBuilder

group :: PrivateGroup
group = fromJust $ makeGroup "consensus"

sendBlock :: Connection -> Block -> IO ()
sendBlock conn block = do
  let msg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = BSL.toStrict $ encode block, outGroups = [Consensus.group], outMsgType = 1}
  send msg conn

listenNetworkBlocks :: (Chan R Message, Connection) -> IO ()
listenNetworkBlocks (chan,conn) = do
  result <- startReceive conn
  msg <- readChan chan
  case msg of
    Just (Regular inMsg) -> print (fromJust . decode . BSL.fromStrict . inData $ inMsg :: Block)
    Nothing -> putStrLn "Bad message..."
    _ -> putStrLn "TODO"
  listenNetworkBlocks (chan,conn)

processRecord :: Connection -> Record -> Bucket -> IO ()
processRecord conn rec bucket = do
  _bucket <- readTVarIO bucket
  either 
    (\bb -> atomically $ writeTVar bucket bb)
    (sendBlock conn)
    (addRec rec _bucket)

name :: IO PrivateName
name = do
  time <- timeCurrent
  rand <- randomIO :: IO Int
  let preH = init $ show time ++ show rand
  let h = hash $ B.pack preH :: Digest SHA256
  return . mkPrivateName . B.pack . show $ h

startConsensus :: Cache -> Bucket -> IO ()
startConsensus cache bucket = do
  n <- Consensus.name
  let config = Conf { address = Just "alcetipe.dyndns.org" , port = Just 4803, desiredName = n, priority = False, groupMembership = True, authMethods = [] }
  (chan,conn) <- connect config
  join Consensus.group conn
  --listener <- Concurrent.forkIO $ listenNetworkBlocks
  listenNetworkBlocks (chan,conn)
