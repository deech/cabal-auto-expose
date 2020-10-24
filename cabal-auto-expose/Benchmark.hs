{-# LANGUAGE BangPatterns #-}
module Main where
import Test.QuickCheck
import Criterion
import Criterion.Main
import Distribution.Simple.AutoExpose hiding (defaultMain)

main :: IO ()
main = do
  let setupEnv = generate (vectorOf 1000 (arbitrary :: Gen String))
  defaultMain [ env setupEnv $ \ ss ->
                  bgroup "main"
                  [ bench "whnf-indexWithNeighbors" $ whnf indexWithNeighbors ss
                  , bench "nf-indexWithNeighbors" $ nf indexWithNeighbors ss
                  ]
              ]
