{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.Wreq
import Control.Lens
import Time.System
import Crypto.Hash
import Data.Aeson (toJSON)
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as B
import Dat
import Control.Concurrent (ThreadId)
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

genName :: TVar Int -> IO String
genName var = do
  time <- timeCurrent
  x <- atomically $ do
      _val <- readTVar var
      writeTVar var (_val+1)
      return _val
  let h = hash $ B.pack $ (show x)++(show time) :: Digest SHA256
  return . show $ h