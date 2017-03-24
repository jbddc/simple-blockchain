module Node.Consensus where

import Spread.Client
import Dat

name :: IO PrivateName
name = do
  time <- timeCurrent
  rand <- randomIO :: IO Int
  let preH = init $ show time ++ show rand
  let h = hash $ B.pack preH :: Digest SHA256
  return $ mkPrivateName $ B.pack $ show h

echoLoop chan = do
  val <- readChan chan
  case val of
    Nothing -> return ()
    Just x -> putStrLn (show x) >> echoLoop chan

messageSender conn = do
  let msg = Outgoing { outOrdering = Fifo, outDiscard = True, outData = B.pack "hello, world", outGroups = [fromJust $ makeGroup "test"], outMsgType = fromInteger 525}
  send msg conn

startNode = do
  n <- name
  let config = Conf { address = Nothing , port = Nothing, desiredName = n, priority = False, groupMembership = True, authMethods = [] }
  (chan,conn) <- connect config
  let group = makeGroup "test"
  if (isJust group) then join (fromJust group) conn else return ()
  --echoLoop chan
  messageSender conn
  putStrLn "Leaving..."