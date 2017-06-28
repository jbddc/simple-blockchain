module Node where

import qualified Database.MongoDB as Mongo
import Serving
import BlockChain
import Block
import Spread.Client
import Consensus
import GHC.Conc
import System.IO
import System.Exit
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TVar

dbConnect = Mongo.connect (Mongo.host "127.0.0.1")

finishDBconn = Mongo.close

nodeStartup = do
  -- get database address
  pipe <- dbConnect
  -- launch consensus part of node (it will be responsible for storing new blocks)
  bls <- consensusHandshake pipe
  n <- Consensus.name
  let config = Conf { address = Consensus.spread_address , port = Consensus.spread_port, desiredName = n, priority = False, groupMembership = True, authMethods = [] }
  spread_con <- connect config
  maybe 
    (do
      _ <- runQuery pipe (insertBlock genesis) 
      cache <- newTVarIO $ mkCache (snd spread_con) 1000 1 genesis 
      serving_tid <- servingVein pipe cache
      consensus_tid <- consensusVein spread_con pipe cache
      listenExit spread_con serving_tid consensus_tid ) 
    (\justBls -> do
      cache <- newTVarIO $ mkCache (snd spread_con) 1000 1 justBls 
      serving_tid <- servingVein pipe cache
      consensus_tid <- consensusVein spread_con pipe cache
      listenExit spread_con serving_tid consensus_tid ) 
    bls

-- | Communicates with other nodes (via spread) to reach a consensus on the BlockChain
consensusVein spread_con pipe cache = forkIO $ startConsensus spread_con pipe cache

servingVein dbPipe cache = do
  -- launch REST API to communicate with clients
  forkIO $ runApiServer dbPipe cache
  -- recieve Transaction requests and pipe them to "transaction bucket" (internal socket connection?)

listenExit spread_con tid1 tid2 = do
  str <- getLine
  spread_alive <- isAlive tid2
  if str == "quit" || str == "exit" || str == "shutdown" || (not spread_alive) then do
    killThread tid1
    killThread tid2
    disconnect $ snd spread_con
  else listenExit spread_con tid1 tid2

isThreadStatusBlocked :: ThreadStatus -> Bool
isThreadStatusBlocked (ThreadBlocked _) = True
isThreadStatusBlocked _ = False

isAlive :: ThreadId -> IO Bool
isAlive = fmap (liftM2 (||) (ThreadRunning ==) isThreadStatusBlocked) . threadStatus

debugVein = undefined
  -- logger with remote access? (http page maybe)
