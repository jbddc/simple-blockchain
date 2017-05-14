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
        get "/hello" $ do
            json genesis
