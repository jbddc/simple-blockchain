{-# LANGUAGE OverloadedStrings #-}

module Serving where

import Block
import Data.Maybe
import Data.Aeson (encode)
import Web.Scotty
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.HTTP.Types.Status (ok200, internalServerError500, notFound404)
import BlockChain
--import Web.Scotty.TLS

runApiServerHTTPS = undefined 

password = "user"

runApiServer pipe cache = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        middleware $ basicAuth (\u p -> return $ u == "user" && p == password) "BSW Server"

        get "/login" $ do
            status ok200
        
        -- hello world
        get "/echo/:val" $ do
            id <- param "val"
            either (liftAndCatchIO . print) text (parseParam id)
        

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
            either (\_ -> status internalServerError500) (\x -> do {res <- liftAndCatchIO $ runQuery pipe $ getBlockByHash x;json res;}) (parseParam blockHash)
