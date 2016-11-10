module Streamer.Prelude where

import Control.Concurrent.MVar

updateMVar :: (a -> a) -> MVar a -> IO ()
updateMVar f var = modifyMVar_ var (return . f)

(#) :: a -> (a -> b) -> b
o # f = f o

(&) :: (a -> b) -> (b -> c) -> a -> c
(&) = flip (.)

whenJust :: (a -> IO ()) -> Maybe a -> IO ()
whenJust = maybe (return ())

whenJustMaybe :: (a -> IO (Maybe b)) -> Maybe a -> IO (Maybe b)
whenJustMaybe = maybe (return Nothing)
