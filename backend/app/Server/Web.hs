{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Web where

import Data.String (fromString)

import Web.Scotty

import Network.HTTP.Types                   (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Server.Types
import Server.ProgramOptions (ProgramOptions(..))
import Server.Stream         (getStream)
import Server.HLSProxy       (getProxy)

import Control.Monad.IO.Class  (liftIO)
import Control.Concurrent.MVar (MVar(..))

runServer :: MVar ServerState -> ProgramOptions -> IO ()
runServer state options = scotty 8000 $ do
  middleware logStdoutDev

  get "/:channel" $ do
    channel <- param "channel"

    maybeStream <- liftIO $ getStream options channel state

    withJustOr404 json maybeStream

  get playlistRoutePattern $ do
    channel <- param "channel"
    quality <- param "quality"

    mbProxy <- liftIO $ getProxy options channel quality state

    withJustOr404 serveProxy mbProxy

  where
    ProgramOptions{indexFileName} = options

    playlistRoutePattern = fromString $
      "/:channel/:quality/" ++ indexFileName

    withJustOr404 :: (a -> ActionM ()) -> Maybe a -> ActionM ()
    withJustOr404 = maybe $ status status404

    serveProxy :: HLSProxy -> ActionM ()
    serveProxy proxy = do
      addHeader "Content-Type" "application/vnd.apple.mpegurl"
      file $ indexPath proxy
