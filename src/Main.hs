module Main where

import Node
import System.Environment

main = do
    args <- getArgs
    if "local" `elem` args then nodeStartup local_address else nodeStartup remote_address