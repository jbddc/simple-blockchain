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
import System.Random
import Time.System
import System.IO.Unsafe
import System.IO
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as TEnc
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64 as BSE (decode)
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
               (const $ status badRequest400) 
               (\x -> do 
                   res <- liftAndCatchIO $ fetchUser cache pipe x
                   maybe (status notFound404) (json) res
               ) 
               (parseParam uname)

        get "/users/:username/:block" $ do
            uname <- param "username"
            block <- param "block"
            either 
                (const $ status badRequest400)
                (\x -> either (const $ status badRequest400) (\y -> liftAndCatchIO $ putStrLn $ "Uname: "++x++" Block: "++y) (parseParam block))
                (parseParam uname)

        -- get group by identifier
        get "/groups/:identifier" $ do
           ident' <- param "identifier"
           either 
               (const $ status badRequest400) 
               (\x -> do 
                   res <- liftAndCatchIO $ fetchGroup cache pipe x
                   maybe (status notFound404) (json) res
               ) 
               (parseParam ident')

        get "/groups/:identifier/:block" $ do
            ident' <- param "identifier"
            block <- param "block"
            either 
                (const $ status badRequest400)
                (\x -> either (const $ status badRequest400) (\y -> liftAndCatchIO $ putStrLn $ "Uname: "++x++" Block: "++y) (parseParam block))
                (parseParam ident')
       
        post "/groupreg" $ do
           greg <- Just `fmap` (jsonData :: ActionM GroupRegister) `rescue` (\_ -> status badRequest400 >> return Nothing)
           case greg of
               Just x -> do
                   n <- liftAndCatchIO $ randGroupId
                   (liftAndCatchIO $ registerGroup cache pipe (x { identifier = (gname x)++n}))
                   status ok200
               Nothing -> return ()

        post "/addfriend" $ do
           af <- Just `fmap` (jsonData :: ActionM AddFriend) `rescue` (\_ -> status badRequest400 >> return Nothing)
           case af of
               Just x -> do
                   h <- header "Authorization" 
                   let auth_header = maybe (Left "") (BSE.decode . B.toStrict . TEnc.encodeUtf8 . Text.drop 1 . Text.dropWhile (/=' ')) h
                   either 
                     (const (status badRequest400)) 
                     (\str -> do
                         let uname = takeWhile (/=':') $ BS.unpack str
                         if uname/=(user_id x) then (status badRequest400) else (liftAndCatchIO $ addFriend cache pipe x) >>= (\res -> if res then status ok200 else status badRequest400)
                     ) 
                     auth_header
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


randGroupId :: IO String
randGroupId = do
  time <- timeCurrent
  rand <- randomRIO (0,500) :: IO Int
  return $ show time ++ show rand