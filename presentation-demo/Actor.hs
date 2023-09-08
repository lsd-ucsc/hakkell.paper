module Actor
( Reexports.forkIO
, Reexports.killThread
, send
, runActor

, say
) where

import qualified Control.Concurrent as Reexports

import Control.Exception (Exception(..), throwTo, mask_, catch)
import Control.Concurrent (ThreadId, threadDelay, myThreadId)

send :: Exception a => ThreadId -> a -> IO ()
send receiver message = do
    self <- myThreadId
    say $ "sending " ++ show message ++ " to " ++ show receiver
    throwTo receiver message

runActor :: Exception a => (st -> a -> IO st) -> st -> IO ()
runActor consume s₀ = do
    mask_ $ loop (s₀, [])
  where
    loop (state, inbox) = do
        catch
            (case inbox of
                [] -> do
                    threadDelay 66000000
                    return (state, inbox)
                x:xs -> do
                    (,) <$> consume state x
                        <*> return xs)
            $ \e -> do
                return (state, inbox ++ [e])
        >>= loop

say :: String -> IO ()
say out = do
    self <- myThreadId
    putStrLn $ show self ++ ": " ++ out
