{-# LANGUAGE NamedFieldPuns #-}

module Server.Stream where

import Prelude hiding (lookup)

import Streamer.Prelude

import Data.Map  (lookup, insert, delete, keys)
import Data.List (find)
import Data.Time (getCurrentTime, UTCTime(..))
import Data.Char (toLower)

import Control.Concurrent

import System.Environment (lookupEnv)

import Server.Types

import Twitch.API
import Twitch.Types

insertStream :: Stream -> ServerState -> ServerState
insertStream stream state@ServerState{streams} =
  state { streams = insert (stream # channel) stream streams }

deleteStream :: ChannelName -> ServerState -> ServerState
deleteStream channel state@ServerState{streams} =
  state { streams = delete channel streams }

fetchStream :: ChannelName -> IO (Maybe Stream)
fetchStream channel = do
  mbClientID <- getTwitchClientID

  whenJustMaybe fetchStreamWithClientID mbClientID

  where
    getTwitchClientID :: IO (Maybe String)
    getTwitchClientID = lookupEnv "TWITCH_CLIENT_ID"

    fetchStreamWithClientID :: String -> IO (Maybe Stream)
    fetchStreamWithClientID clientID = do
      mbPlaylists <- streamPlaylists clientID channel
      now <- getCurrentTime

      return $ Stream
        <$> Just channel
        <*> mbPlaylists
        <*> Just now

getStream :: ChannelName -> MVar ServerState -> IO (Maybe Stream)
getStream rawChannel state = do
  ServerState{streams} <- readMVar state
  -- Check if there is an existing stream
  case lookup channel streams of
    -- Stream found, return it
    Just stream -> return $ Just stream
    -- Otherwise create it
    Nothing     -> createStream

  where
    channel = map toLower rawChannel

    createStream :: IO (Maybe Stream)
    createStream = do
      mbStream <- fetchStream channel
      whenJust monitorAndStoreStream mbStream
      return mbStream

    monitorAndStoreStream :: Stream -> IO ()
    monitorAndStoreStream stream = do
      forkIO monitorStream
      updateMVar (insertStream stream) state

    streamTimeout = 30

    monitorStream :: IO ()
    monitorStream = do
      threadDelay (streamTimeout * 1000000)
      ServerState{proxies} <- readMVar state
      -- Check if there is a proxy running for the channel
      case find (fst & (== channel)) (keys proxies) of
        -- Proxy found, try again in a bit
        Just _  -> monitorStream
        -- Otherwise expire cache
        Nothing -> updateMVar (deleteStream channel) state
