{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serving where

import Block
import Dat
import Data.Maybe
import Data.Aeson (encode)
import Web.Scotty
import Network.Wai.Middleware.HttpAuth (basicAuth')
import Network.Wai.Internal (pathInfo)
import Network.HTTP.Types.Status (ok200, accepted202, badRequest400, notFound404, conflict409, internalServerError500)
import BlockChain
import Control.Monad
import System.IO.Unsafe
import System.IO
import qualified Data.Text.Lazy as Text
import qualified Data.ByteString.Char8 as BS
--import Web.Scotty.TLS

runApiServer pipe cache = do
    putStrLn "Starting Server..."
    scotty 9000 $ do
        middleware $ basicAuth' (\req u p -> if fromMaybe "" (listToMaybe (pathInfo req)) == "newuser" then return True else userLogin cache pipe (BS.unpack u) (BS.unpack p)) "BSW Server"

        post "/newuser" $ do
            usr <- Just `fmap` (jsonData :: ActionM UserRegister) `rescue` (\_ -> status badRequest400 >> return Nothing)
            case usr of
                Just x -> do
                    res <- liftAndCatchIO $ regUser cache pipe x
                    if res then status ok200 else status conflict409
                Nothing -> return ()
        
        -- get user by username
        get "/users/:username" $ do
           uname <- param "username"
           either 
               (const $ status notFound404) 
               (\x -> do 
                   res <- liftAndCatchIO $ fetchUser cache pipe x
                   maybe (status notFound404) (json) res
               ) 
               (parseParam uname)
       
        post "/groupreg" $ do
           greg <- Just `fmap` (jsonData :: ActionM GroupRegister) `rescue` (\_ -> status badRequest400 >> return Nothing)
           case greg of
               Just x -> (liftAndCatchIO $ registerGroup cache pipe x) >> status ok200
               Nothing -> return ()

        -- get block by index
        get "/blockchain/byIndex/:blockIndex" $ do
            blockIndex <- param "blockIndex"
            either 
              (const $ status internalServerError500) 
              (\x -> do 
                  res <- liftAndCatchIO $ runQuery pipe $ getBlockByIndex x
                  json res
              ) 
              (parseParam blockIndex)
        
        -- get block by hash code 
        get "/blockchain/byHash/:blockHash" $ do
            blockHash <- param "blockHash"
            either (\_ -> status internalServerError500) (\x -> do {res <- liftAndCatchIO $ runQuery pipe $ getBlockByHash x;maybe (status notFound404) (\x -> json x) res;}) (parseParam blockHash)
