{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.Wreq
import Control.Lens
import Time.System
import Crypto.Hash
import Data.Aeson (toJSON,decode)
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Dat
import User
import Control.Concurrent (ThreadId, threadDelay)
import Control.Exception.Base
import Control.Concurrent.Thread

default_path = "http://localhost:9000"

tester :: Int -> Int -> String -> IO ()
tester threads each path = do
    var <- newTVarIO 0
    _id_vals <- replicateM threads (forkIO $ test each path var)
    id_vals <- mapM (\(tid,comp) -> do { thread_res <- comp ; treat_thread_res tid thread_res }) _id_vals
    let total_time = sum $ map snd id_vals
    let total_res = sum $ map fst id_vals
    putStrLn $ "Succeded "++(show total_res)++"/"++(show (each*threads))++"requests."
    putStrLn $ "Took "++(show total_time)++" seconds."

treat_thread_res :: ThreadId -> Either SomeException (Int,Int) -> IO (Int,Int)
treat_thread_res tid r = either (\exp -> print exp >> return (-1,-1)) (\res -> auxi tid res) r

auxi tid res = do 
    putStrLn $ "Thread "++(tail $ dropWhile (/=' ') $ show tid)++" executed "++(show $ fst res)++" successfully in "++(show $ snd res)++" seconds."
    return res


test :: Int -> String -> TVar Int -> IO (Int,Int)
test n path var = do
    before <- (read . init . show) `fmap` timeCurrent
    names <- replicateM n (genName var)
    let _results = foldr (\name accum -> (name,newUserReq path name):accum) [] names
    results <- mapM (\(a,b) -> do { _b <- b ; return (a,_b)}) _results
    ress <- mapM (\r -> do {return $ r ^. responseStatus . statusCode }) $ map snd results
    let total = filter (==(fromIntegral 200)) ress
    after <- (read . init . show) `fmap` timeCurrent
    let timing = after - before 
    return (length total,timing)

newUserReq path name = do
    let opts = defaults & auth ?~ basicAuth "user" "pass"
        usr = UserRegister name name
    postWith opts (path++"/newuser") (toJSON usr) 

newGroupReg me groupMem path = do
    let opts = defaults & auth ?~ basicAuth (B.pack me) (B.pack me) 
        grp = GroupRegister groupMem "group1" "group1" "group example"
    postWith opts (path++"/groupreg") (toJSON grp)

newTransaction tran path = do
    let opts = defaults & auth ?~ basicAuth (B.pack $ usr tran) (B.pack $ usr tran) 
    postWith opts (path++"/newtransaction") (toJSON tran)

fetchUsr n path = do
    let opts = defaults & auth ?~ basicAuth (B.pack n) (B.pack n) 
    res <- getWith opts (path++"/users/"++n) 
    let result = decode (res ^. responseBody) :: Maybe UserResponse
    return $ maybe (UserResponse n [] [] "-1") id result

genName :: TVar Int -> IO String
genName var = do
  time <- timeCurrent
  x <- atomically $ do
      _val <- readTVar var
      writeTVar var (_val+1)
      return _val
  let h = hash $ B.pack $ (show x)++(show time) :: Digest SHA256
  return . show $ h

worker :: [String] -> TVar Bool -> String -> String -> IO Int
worker groupMembers groupCreated name path = do
    before <- (read . init . show) `fmap` timeCurrent
    checkGC <- atomically $ do
        _val <- readTVar groupCreated
        if _val==True then return _val else writeTVar groupCreated True >> return False
    if checkGC==False 
        then do 
            newGroupReg name groupMembers path
            usr <- usrFetcher name path
            ts <- timeCurrent
            newTransaction (TransactionReg name (head $ groups usr) [(ValueByUser name 2)] "msg" (show ts)) path
            after <- (read . init . show) `fmap` timeCurrent
            return $ after - before
        else do 
            usr <- usrFetcher name path
            ts <- timeCurrent
            newTransaction (TransactionReg name (head $ groups usr) [(ValueByUser name 2)] "msg" (show ts)) path
            after <- (read . init . show) `fmap` timeCurrent
            return $ after - before

usrFetcher name path = do
    usr <- fetchUsr name path
    if (length $ groups usr)==0 then threadDelay 5 >> usrFetcher name path else return usr

tester2 :: Int -> Int -> String -> IO ()
tester2 threads each path = do
    var <- newTVarIO 0
    tids_comps <- replicateM threads (forkIO $ launchTest2 each path var) 
    _res <- mapM (\r -> do {_r <- r ; return $ either (error . show) id _r}) $ map snd tids_comps
    putStrLn "All workers: OK!"
    putStrLn $ "Total Time: "++(show $ sum _res)++" seconds."
    putStrLn $ "Time per request: "++(show $ (sum _res) `div` (threads*each))++" seconds."

launchTest2 :: Int -> String -> TVar Int -> IO Int
launchTest2 each path var = do 
    names <- replicateM each (genName var)
    mapM_ (newUserReq path) names
    var <- newTVarIO False
    res <- mapM (\n -> forkIO $ worker names var n path) names
    _res <- mapM (\r -> do { _r <- r ; return $ either (error . show) id _r}) $ map snd res 
    return $ sum _res
