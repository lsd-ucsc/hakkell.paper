{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Actor (forkIO, killThread, send, runActor, say)

import Control.Exception (Exception)
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import System.Environment (getArgs)
import System.Random (RandomGen, randomR, getStdRandom)


main :: IO ()
main = do
    [nstr] <- getArgs
    let n = read nstr
    ringElection n $
        runActor node Uninitialized


data State
    = Uninitialized
    | Member {next::ThreadId}

data Message
    = Init{next::ThreadId}
    | Start
    | Nominate{nominee::ThreadId}
    deriving Show

instance Exception Message


node :: State -> Message -> IO State
node
    Uninitialized
    Init{next} = do
        return Member{next}
node
    Member{next}
    Start = do
        send next . Nominate =<< myThreadId
        return Member{next}
node
    Member{next}
    Nominate{nominee} = do
        self <- myThreadId
        if  | self == nominee -> say "I win"
            | self <  nominee -> send next Nominate{nominee}
            | otherwise       -> send next Nominate{nominee=self}
        return Member{next}
node _ _ =
    error "unhandled message"


-- | Set up some threads with a ring-topology
ringElection :: Int -> IO () -> IO ()
ringElection n actor = do

    -- Fork `n` threads running `actor`
    nodes <- sequence . replicate n . forkIO $ actor

    -- Shuffle the list of threads
    ring <- getStdRandom $ permute nodes
    print ring

    -- Tell each thread its successor
    mapM_ (\(self, next) -> send self Init{next})
        $ zip ring (tail ring ++ [head ring])

    -- Tell a thread to start
    send (head ring) Start

    -- Wait and then kill
    threadDelay 1000000
    mapM_ killThread ring

-- | Draw random elements from the pool to construct a permutation.
permute :: RandomGen g => [a] -> g -> ([a], g)
permute pool0 gen0
    = snd
    . foldr pick (pool0, ([], gen0))
    $ replicate (length pool0) ()
  where
    pick () (pool, (output, g)) =
        let (index, g') = randomR (0, length pool - 1) g
            (x, pool') = pop pool index
        in (pool', (x:output, g'))
    pop (x:xs) 0 = (x, xs)
    pop (x:xs) n = (x:) <$> pop xs (n - 1)
    pop [] _ = error "pop empty list"
