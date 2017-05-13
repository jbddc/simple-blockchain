module BlockChain where

import Block
import Dat

dbName = "bsw_blockchain"

type Chain = [Block]

blockchain = [genesis]

addBlock :: Block -> Chain -> Chain
addBlock x l = x:l

lastBlock :: Chain -> Block
lastBlock = head

numBlocks :: Chain -> Integer
numBlocks = undefined

getBlock :: Chain -> Integer -> Block
getBlock = undefined
