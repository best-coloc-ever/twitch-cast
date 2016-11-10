module Server.ProgramOptions where

import Server.Types

import System.Environment (lookupEnv)

import Options.Applicative

data ProgramOptions = ProgramOptions
  { ffmpegPath :: String
  , dataDirectory :: String
  , indexFileName :: String
  , twitchClientID :: String
  , streamInfoTimeout :: Int
  , proxyTimeout :: Int
  , tsSegmentLength :: Int
  , tsSegmentCount :: Int
  } deriving (Show)

parseOptions :: IO ProgramOptions
parseOptions = do
  defaults <- getDefaultOptions

  execParser $
    info (helper <*> (programOptionsParser defaults))
    (  fullDesc
    <> progDesc "Backend application for twitch-cast (https://datcoloc.com/twitch-cast)"
    <> header   "twitch-cast streamer"
    )

  where
    getDefaultOptions :: IO ProgramOptions
    getDefaultOptions = ProgramOptions
      <$> environment     "FFMPEG_PATH"         ""
      <*> environment     "DATA_DIRECTORY"      "/tmp/video"
      <*> environment     "INDEX_FILE_NAME"     "index.m3u8"
      <*> environment     "TWITCH_CLIENT_ID"    ""
      <*> environmentRead "STREAM_INFO_TIMEOUT" 30
      <*> environmentRead "PROXY_TIMEOUT"       60
      <*> environmentRead "TS_SEGMENT_LENGTH"   4
      <*> environmentRead "TS_SEGMENT_COUNT"    4

    environment :: String -> String -> IO String
    environment key def = do
      envValue <- lookupEnv key
      return $ maybe def id envValue

    environmentRead :: Read a => String -> a -> IO a
    environmentRead key def = do
      envValue <- lookupEnv key
      return $ maybe def read envValue

programOptionsParser :: ProgramOptions -> Parser ProgramOptions
programOptionsParser defaults = ProgramOptions
  <$> strOption
      (  long "ffmpeg-path" <> short 'f'
      <> value (ffmpegPath defaults)
      <> metavar "PATH"
      <> help "Path to the ffmpeg binary"
      )
  <*> strOption
      (  long "data-dir" <> short 'd'
      <> value (dataDirectory defaults)
      <> metavar "DIR"
      <> help "Root directory to dump video data in"
      )
  <*> strOption
      (  long "index-file-name" <> short 'i'
      <> value (indexFileName defaults)
      <> metavar "NAME"
      <> help "Name of the playlist files"
      )
  <*> strOption
      (  long "twitch-client-id" <> short 't'
      <> value (twitchClientID defaults)
      <> metavar "ID"
      <> help "The ID of your registered twitch application"
      )
  <*> option auto
      (  long "stream-info-timeout" <> short 's'
      <> value (streamInfoTimeout defaults)
      <> metavar "SECONDS"
      <> help "The minimum lifetime in seconds of stream information in the cache"
      )
  <*> option auto
      (  long "proxy-timeout" <> short 'p'
      <> value (proxyTimeout defaults)
      <> metavar "SECONDS"
      <> help "The maximum lifetime of an unused video proxy"
      )
  <*> option auto
      (  long "ts-length"
      <> value (tsSegmentLength defaults)
      <> metavar "SECONDS"
      <> help "The length in seconds of proxied video data segments"
      )
  <*> option auto
      (  long "ts-count"
      <> value (tsSegmentCount defaults)
      <> metavar "NUM"
      <> help "The number of proxied video data segments referenced in the playlist"
      )
