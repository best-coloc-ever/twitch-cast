module Twitch.Types where

type ChannelName = String

data AccessToken = AccessToken
  { tokenValue :: String
  , signature  :: String
  } deriving (Show)

data Resolution = Resolution
  { width  :: Int
  , height :: Int
  } deriving (Show)

data HLSPlaylist = HLSPlaylist
  { bandwidth  :: Int
  , resolution :: Resolution
  , name       :: String
  , url        :: String
  } deriving (Show)
