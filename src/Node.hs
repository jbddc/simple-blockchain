module Node where

import Database.MongoDB

dbConnect = connect (host "127.0.0.1")

finishDBconn = close

nodeStartup = do
  -- get database address
  pipe <- dbConnect
  -- launch consensus part of node (it will be responsible for storing new blocks)
  servingId <- servingVein pipe
  -- launch "transaction bucket" part of node. It will create a new block when it reaches a threshold and publish it to the network
  -- lanch debug vein that allows interaction with connected nodes (check current blockchain state)

consensusVein = undefined
  -- communicates with other nodes (via spread) to reach a consensus on the BlockChain

servingVein = undefined
  -- launch REST API to communicate with clients
  -- recieve Transaction requests and pipe them to "transaction bucket" (internal socket connection?)
  -- handle application logic? (client registring, group registring, client login etc)

debugVein = undefined
  -- logger with remote access? (http page maybe)