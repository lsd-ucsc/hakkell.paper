module Actor
( Reexports.forkIO
, Reexports.killThread
, send
, runActor
) where

import qualified Control.Concurrent as Reexports

import Control.Exception (Exception(..), throwTo, mask_, catch)
import Control.Concurrent (ThreadId, threadDelay)

send :: Exception a => ThreadId -> a -> IO ()
send receiver message = do
    throwTo receiver message

runActor :: Exception a => (a -> IO ()) -> IO ()
runActor consume = do
    mask_ (loop [])
  where
    loop inbox = do
        xs <- catch
            (case inbox of
                []   ->do threadDelay 66666666
                          return []
                x:xs ->do consume x
                          return xs)
            (\e -> return $ inbox<>[e])
        loop xs
