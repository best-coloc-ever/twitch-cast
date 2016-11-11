{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Types where

import System.Process (ProcessHandle)

import Data.Time      (UTCTime)
import Data.Map       (Map, empty)
import Data.Aeson     (ToJSON, toJSON, (.=), object, encode)

import Twitch.Types   (ChannelName, HLSPlaylist(..), Resolution(..))

data Stream = Stream
  { channel   :: ChannelName
  , playlists :: [HLSPlaylist]
  , fetchedAt :: UTCTime
  }

data HLSProxy = HLSProxy
  { processHandle :: ProcessHandle
  , indexPath     :: String
  , lastUsedAt    :: UTCTime
  }

type PlaylistName = String
type ProxyKey = (ChannelName, PlaylistName)

data ServerState = ServerState
  { streams :: Map ChannelName Stream
  , proxies :: Map ProxyKey HLSProxy
  }

newServerState :: ServerState
newServerState = ServerState
  { streams = empty
  , proxies = empty
  }

instance ToJSON Stream where
  toJSON Stream{..} = object
    [ "channel"   .= channel
    , "playlists" .= playlists
    ]

instance ToJSON HLSPlaylist where
  toJSON HLSPlaylist{..} = object
    [ "bandwidth"  .= bandwidth
    , "resolution" .= resolution
    , "name"       .= name
    , "url"        .= url
    ]

instance ToJSON Resolution where
  toJSON Resolution{..} = object
   [ "width"  .= width
   , "height" .= height
   ]
