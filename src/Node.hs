module Node where

import Database.MongoDB
import Serving
import BlockChain
import Block
import Consensus
import Control.Concurrent.STM.TVar

dbConnect = connect (host "127.0.0.1")

finishDBconn = close

nodeStartup = do
  -- get database address
  pipe <- dbConnect
  -- launch consensus part of node (it will be responsible for storing new blocks)
  bls <- consensusHandshake

  maybe 
    (do
      _ <- runQuery pipe (insertBlock genesis) 
      cache <- newTVarIO $ mkCache 1000 1 genesis 
      servingVein pipe cache
      consensusVein pipe cache)
    (do
      cache <- newTVarIO $ mkCache 1000 1 (last bls) 
      servingVein pipe cache
      consensusVein pipe cache)
    bls




-- | Communicates with other nodes (via spread) to reach a consensus on the BlockChain
consensusVein = startConsensus

servingVein dbPipe cache = do
  -- launch REST API to communicate with clients
  runApiServer dbPipe cache
  -- recieve Transaction requests and pipe them to "transaction bucket" (internal socket connection?)

debugVein = undefined
  -- logger with remote access? (http page maybe)