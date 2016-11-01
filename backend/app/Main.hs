module Main where

import Twitch.API
import Twitch.Types
import Twitch.Playlist

import System.Environment

import Control.Monad
import Data.List

getTwitchClientID :: IO (Maybe String)
getTwitchClientID = do
  args <- getArgs
  case args of
    clientID:_ -> return $ Just clientID
    [] -> lookupEnv "TWITCH_CLIENT_ID"

main :: IO ()
main = do
  mbClientID <- getTwitchClientID
  case mbClientID of
    Just clientID -> do
      index <- streamIndex clientID "sodapoppin"
      case parseHLSPlaylists index of
        Right indexes -> mapM_ print indexes
        Left error -> print error
    Nothing -> return ()
