module Permute where

import System.Random (RandomGen, randomR, getStdRandom)

permuteIO :: [a] -> IO [a]
permuteIO = getStdRandom . permute

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

