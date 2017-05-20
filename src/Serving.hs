{-# LANGUAGE OverloadedStrings #-}

module Serving where

import Block
import Data.Aeson (encode)
import Web.Scotty
import Web.Scotty.TLS

runApiServerHTTPS = undefined 

runApiServer = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/users/:id" $ do
            --id <- param "id"
            --user <- getUserById id
            json genesis
        get "/hello" $ do
            json genesis
