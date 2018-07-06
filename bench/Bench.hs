module Main where

import           Brainheck
import           Criterion.Main
import qualified Data.Text.IO   as TIO

-- FIXME use nfIO for final result + use env correctly
main :: IO ()
main = TIO.readFile "bf/helloworld.bf" >>= \file -> defaultMain [
    bgroup "parseBrainheck" [
        bench "helloworld.bf" $ whnf (parseBrainheck "bf/helloworld.bf") file ]
    ]
