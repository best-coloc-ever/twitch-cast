{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Web where

import Web.Scotty

import Network.HTTP.Types                   (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Server.Types
import Server.Stream   (getStream)
import Server.HLSProxy (getProxy)

import Control.Monad.IO.Class  (liftIO)
import Control.Concurrent.MVar (MVar(..))

runServer :: MVar ServerState -> IO ()
runServer state = scotty 8000 $ do
  middleware logStdoutDev

  get "/:channel" $ do
    channel <- param "channel"

    maybeStream <- liftIO $ getStream channel state

    withJustOr404 json maybeStream

  get "/:channel/:quality/index.m3u8" $ do
    channel <- param "channel"
    quality <- param "quality"

    mbProxy <- liftIO $ getProxy channel quality state

    withJustOr404 serveProxy mbProxy

  where
    withJustOr404 :: (a -> ActionM ()) -> Maybe a -> ActionM ()
    withJustOr404 = maybe $ status status404

    serveProxy :: HLSProxy -> ActionM ()
    serveProxy proxy = do
      addHeader "Content-Type" "application/vnd.apple.mpegurl"
      file $ indexPath proxy
