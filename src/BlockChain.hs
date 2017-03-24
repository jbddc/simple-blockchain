module BlockChain where

import Block
import Dat

type Chain = [Block]

blockchain = [genesis]

addBlock :: Block -> Chain -> Chain
addBlock x l = x:l  

lastBlock :: Chain -> Block 
lastBlock = head
