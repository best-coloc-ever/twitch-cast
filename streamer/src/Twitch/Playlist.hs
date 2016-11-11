module Twitch.Playlist where

import Twitch.Types

import Data.Char
import Data.List
import Data.Maybe

import Text.Parsec
import Text.Parsec.ByteString.Lazy (Parser)

import qualified Data.ByteString.Lazy as LBS

type PlaylistProperty = (String, String)

playlistPropertyParser :: Parser PlaylistProperty
playlistPropertyParser = do
  let maybeQuote = optional (char '"')
      propertyEnd = maybeQuote >> (char ',' <|> lookAhead newline)

  key <- manyTill anyChar (char '=')
  maybeQuote
  value <- manyTill anyChar propertyEnd
  return (key, value)

integer = read <$> many1 digit

parseBandwidth :: String -> Int
parseBandwidth s = case parse integer "" s of
  Right number -> number
  _ -> 0

parseResolution :: String -> Resolution
parseResolution s = case parse resolutionParser "" s of
  Right res -> res
  _ -> Resolution 0 0

  where
    resolutionParser = do
      width <- integer
      char 'x'
      height <- integer

      return $ Resolution width height

hlsPlaylistParser :: Parser HLSPlaylist
hlsPlaylistParser = do
  manyTill anyChar (try $ string "#EXT-X-MEDIA:")
  mediaProperties <- manyTill playlistPropertyParser newline
  manyTill anyChar (try $ string "#EXT-X-STREAM-INF:")
  infProperties <- manyTill playlistPropertyParser newline
  url <- manyTill anyChar newline

  return $ HLSPlaylist
    { bandwidth  = parseBandwidth $ prop "bandwidth" infProperties
    , resolution = parseResolution $ prop "resolution" infProperties
    , name       = prop "name" mediaProperties
    , url        = url
    }

  where
    prop :: String -> [PlaylistProperty] -> String
    prop name properties = do
      let pred = (== (map toLower name)) . (map toLower) . fst
          match = find pred properties
      fromMaybe "none" (snd <$> match)

parseHLSPlaylists :: LBS.ByteString -> Either ParseError [HLSPlaylist]
parseHLSPlaylists = parse (manyTill hlsPlaylistParser eof) ""
