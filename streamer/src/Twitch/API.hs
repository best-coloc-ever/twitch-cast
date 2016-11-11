{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Twitch.API where

import Streamer.Prelude

import Twitch.Types
import Twitch.Playlist

import Network.HTTP.Simple

import Data.Aeson
import Data.Char
import Data.Either.Combinators

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Control.Exception
import Control.Monad

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken <$>
                         v .: "token" <*>
                         v .: "sig"

type ClientID = String

channelAccessTokenUrl channel =
     "http://api.twitch.tv/api/channels/"
  ++ map toLower channel
  ++ "/access_token"

channelAccessToken :: ClientID -> ChannelName -> IO (Maybe AccessToken)
channelAccessToken clientID channel = do
  request <- addRequestHeader "Client-ID" (BS.pack clientID) <$>
             parseRequest (channelAccessTokenUrl channel)

  response <- httpJSONEither request

  return $ rightToMaybe $ getResponseBody response

streamIndexUrl channel =
     "http://usher.twitch.tv/api/channel/hls/"
  ++ map toLower channel
  ++ ".m3u8"

streamIndexQueryString :: AccessToken -> [(BS.ByteString, Maybe BS.ByteString)]
streamIndexQueryString AccessToken{tokenValue, signature} =
  [ ("token",        Just $ BS.pack tokenValue)
  , ("sig",          Just $ BS.pack signature)
  , ("allow_source", Just "true")
  , ("type",         Just "any")
  ]

streamPlaylists :: ClientID -> ChannelName -> IO (Maybe [HLSPlaylist])
streamPlaylists clientID channel = do
  mbToken <- channelAccessToken clientID channel

  whenJustMaybe performRequestWithToken mbToken

  where
    performRequestWithToken :: AccessToken -> IO (Maybe [HLSPlaylist])
    performRequestWithToken token = do
      request <- setRequestQueryString (streamIndexQueryString token) <$>
                 parseRequest (streamIndexUrl channel)

      mbResponse <- (Just <$> httpLBS request) `catch` nothingOnException

      return $ join (parsePlaylistsWithResponse <$> mbResponse)

    parsePlaylistsWithResponse :: Response LBS.ByteString -> Maybe [HLSPlaylist]
    parsePlaylistsWithResponse response = do
      let eitherPlaylists = parseHLSPlaylists $ getResponseBody response
      rightToMaybe eitherPlaylists

    nothingOnException :: HttpException -> IO (Maybe a)
    nothingOnException = const $ return Nothing
