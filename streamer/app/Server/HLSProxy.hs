{-# LANGUAGE NamedFieldPuns #-}

module Server.HLSProxy where

import Prelude hiding (lookup)

import Streamer.Prelude

import Data.Map  (lookup, insert, delete, adjust)
import Data.List (find, intercalate)
import Data.Time (UTCTime(..), getCurrentTime, NominalDiffTime(..), diffUTCTime)
import Data.Char (toLower)
import Data.Maybe (isJust)

import Control.Concurrent
import Control.Concurrent.MVar

import System.Process
import System.Directory

import Server.Types
import Server.Stream
import Server.ProgramOptions

import Twitch.API
import Twitch.Types

insertProxy :: ProxyKey -> HLSProxy -> ServerState -> ServerState
insertProxy proxyKey proxy state@ServerState{proxies} =
  state { proxies = insert proxyKey proxy proxies }

deleteProxy :: ProxyKey -> ServerState -> ServerState
deleteProxy proxyKey state@ServerState{proxies} =
  state { proxies = delete proxyKey proxies }

updateProxy :: ProxyKey -> ServerState -> IO ServerState
updateProxy proxyKey state@ServerState{proxies} = do
  now <- getCurrentTime
  let newProxies = adjust (updateProxyLastUsed now) proxyKey proxies
  return $ state { proxies = newProxies }

  where
    updateProxyLastUsed :: UTCTime -> HLSProxy -> HLSProxy
    updateProxyLastUsed time proxy = proxy { lastUsedAt = time }

getProxy :: ProgramOptions -> ChannelName -> PlaylistName -> MVar ServerState -> IO (Maybe HLSProxy)
getProxy options rawChannel rawQuality state = do
  ServerState{proxies} <- readMVar state
  -- Check if there is a proxy running for the channel
  case lookup proxyKey proxies of
    -- Proxy found, update its last used and return it
    Just proxy -> do
      modifyMVar_ state (updateProxy proxyKey)
      return $ Just proxy
    -- No proxy found, get stream information and try creating a proxy
    Nothing    -> do
      mbStream <- getStream options channel state
      whenJustMaybe createProxyFromStream mbStream

  where
    ProgramOptions
      { ffmpegPath
      , dataDirectory, indexFileName
      , tsSegmentLength, tsSegmentCount
      , proxyTimeout
      } = options

    channel = map toLower rawChannel
    quality = map toLower rawQuality
    proxyKey = (channel, quality)

    createProxyFromStream :: Stream -> IO (Maybe HLSProxy)
    createProxyFromStream Stream{playlists} = do
      -- Create a proxy if a matching playlist exists
      let mbPlaylist = find byLowerCasedQuality playlists
      whenJustMaybe createProxy mbPlaylist

      where
        byLowerCasedQuality :: HLSPlaylist -> Bool
        byLowerCasedQuality = name & (map toLower) & (== quality)

    directory = intercalate "/" [dataDirectory, channel, quality]
    indexPath = intercalate "/" [directory, indexFileName]

    createProxy :: HLSPlaylist -> IO (Maybe HLSProxy)
    createProxy HLSPlaylist{url} = do
      -- Prepare FS and spawn the proxy process
      createDirectoryIfMissing True directory
      (_, _, _, handle) <- createProcess $ proxyProcess url

      now <- getCurrentTime
      let proxy = HLSProxy handle indexPath now
      -- Store and monitor the proxy
      updateMVar (insertProxy proxyKey proxy) state
      forkIO monitorProxy
      -- Wait for the index to be created
      waitForIndex

      return $ Just proxy

    proxyProcess :: String -> CreateProcess
    proxyProcess indexUrl = do
      proc ffmpegPath
        [ "-i",                  indexUrl
        , "-codec",              "copy"
        , "-segment_list_flags", "+live"
        , "-hls_list_size",      show tsSegmentCount
        , "-hls_time",           show tsSegmentLength
        , "-hls_flags",          "delete_segments"
        , indexPath
        ]

    fsPollInterval = 250000 -- microseconds
    maxIndexWaitTime = 8 * 1000000

    waitForIndex :: IO ()
    waitForIndex = go 0
      where
        go :: Int -> IO ()
        go elapsed = do
          threadDelay fsPollInterval
          fileCreated <- doesFileExist indexPath

          let timedOut = elapsed >= maxIndexWaitTime

          if timedOut || fileCreated
            then return ()
            else go $ elapsed + fsPollInterval

    monitorProxy :: IO ()
    monitorProxy  = do
      ServerState{proxies} <- readMVar state
      -- Check proxy timeout if it still exists
      let mbProxy = lookup proxyKey proxies
      whenJust checkTimeoutOrExit mbProxy

    checkTimeoutOrExit :: HLSProxy -> IO ()
    checkTimeoutOrExit HLSProxy{processHandle, lastUsedAt} = do
      now <- getCurrentTime
      mbExit <- getProcessExitCode processHandle

      let remaining = diffUTCTime now lastUsedAt
          expiresIn = (fromIntegral proxyTimeout) - remaining
          expired = expiresIn <= 0
          exited = isJust mbExit

      if expired || exited
        then removeProxy processHandle
        else checkLater expiresIn

    removeProxy :: ProcessHandle -> IO ()
    removeProxy handle = do
      -- Despawn process
      terminateProcess handle
      waitForProcess handle
      -- Cleanup FS
      removeDirectoryRecursive directory
      -- Update state
      updateMVar (deleteProxy proxyKey) state

    checkLater :: NominalDiffTime -> IO ()
    checkLater seconds = do
      threadDelay $ round (seconds * 1000000)
      monitorProxy
