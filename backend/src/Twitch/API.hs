{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Twitch.API where

import Twitch.Types

import Network.HTTP.Simple

import Data.Aeson
import Data.Char

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken <$>
                         v .: "token" <*>
                         v .: "sig"

type ClientID = String

channelAccessTokenUrl channel =
     "http://api.twitch.tv/api/channels/"
  ++ map toLower channel
  ++ "/access_token"

channelAccessToken :: ClientID -> ChannelName -> IO AccessToken
channelAccessToken clientID channel = do
  request <- addRequestHeader "Client-ID" (BS.pack clientID) <$>
             parseRequest (channelAccessTokenUrl channel)

  response <- httpJSON request

  return $ getResponseBody response

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

streamIndex :: ClientID -> ChannelName -> IO LBS.ByteString
streamIndex clientID channel = do
  token <- channelAccessToken clientID channel

  request <- setRequestQueryString (streamIndexQueryString token) <$>
             parseRequest (streamIndexUrl channel)

  response <- httpLBS request

  return $ getResponseBody response
