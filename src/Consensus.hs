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

group :: PrivateGroup
group = fromJust $ makeGroup "consensus"

sendBlock :: Block -> Connection -> IO ()
sendBlock block conn = do
  let msg = Outgoing {outOrdering = Fifo, outDiscard = True, outData = BSL.toStrict $ encode block, outGroups = [group], outMsgType = 1}
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

name :: IO PrivateName
name = do
  time <- timeCurrent
  rand <- randomIO :: IO Int
  let preH = init $ show time ++ show rand
  let h = hash $ B.pack preH :: Digest SHA256
  return . mkPrivateName . B.pack . show $ h

startConsensus :: IO ()
startConsensus = do
  n <- name
  let config = Conf { address = Nothing , port = Nothing, desiredName = n, priority = False, groupMembership = True, authMethods = [] }
  (chan,conn) <- connect config
  join group conn
  --listener <- Concurrent.forkIO $ listenNetworkBlocks
  listenNetworkBlocks (chan,conn)
