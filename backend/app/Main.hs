module Main where

import Control.Concurrent.MVar (newMVar)

import Server.Web              (runServer)
import Server.Types            (newServerState)

main :: IO ()
main = do
  serverState <- newMVar newServerState

  runServer serverState
