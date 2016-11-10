module Main where

import Control.Concurrent.MVar (newMVar)

import Server.Web              (runServer)
import Server.Types            (newServerState)
import Server.ProgramOptions   (parseOptions)

main :: IO ()
main = do
  programOptions <- parseOptions
  serverState <- newMVar newServerState

  runServer serverState programOptions
