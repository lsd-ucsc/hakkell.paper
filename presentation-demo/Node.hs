{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Actor (forkIO, killThread, send, runActor)
import Permute (permuteIO)

import System.Environment (getArgs)
import Control.Exception (Exception)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Node
    = Uninitialized
    | Member {next::ThreadId}

data Msg
    = Init{next::ThreadId}
    | Start
    | Nominate{nominee::ThreadId}
    deriving Show

instance Exception Msg


node :: Msg -> Node -> IO Node
node
  Init{next}
  Uninitialized = do
    say $ "next is " ++ show next
    return Member{next}
node
  Start
  state@Member{next} = do
    self <- myThreadId
    say $ "start, nominating myself to " ++ show next
    send next Nominate{nominee=self}
    return state
node
  msg@Nominate{nominee}
  state@Member{next} = do
    self <- myThreadId
    if  | self == nominee -> do say "I win"
        | self <  nominee -> do say $ "forward " ++ show msg ++ " to " ++ show next
                                send next msg
        | otherwise       -> do say $ "override, nominating myself to " ++ show next
                                send next Nominate{nominee=self}
    return state
node _ _ =
    error "unhandled call"


ringElection :: Int -> IO () -> IO ()
ringElection n actor = do
    nodes <- sequence . replicate n . forkIO $ actor
    ring <- Permute.permuteIO nodes
    print ring
    mapM_ (\(self, next) -> send self Init{next})
        $ zip ring (tail ring ++ [head ring])
    send (head ring) Start
    threadDelay 1000000
    mapM_ killThread ring

main :: IO ()
main = do
    [n] <- getArgs
    ringElection (read n) $ do
        ref <- newIORef Uninitialized
        runActor (stateful ref node)

stateful :: IORef st -> (a -> st -> IO st) -> a -> IO ()
stateful ref consume msg = do
    writeIORef ref =<< consume msg =<< readIORef ref

say :: String -> IO ()
say out = do
    self <- myThreadId
    putStrLn $ show self ++ ": " ++ out
