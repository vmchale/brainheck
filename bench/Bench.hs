module Main where

import Criterion.Main
import Brainheck
import qualified Data.Text.IO as TIO

-- FIXME use nfIO for final result + use env correctly
main = TIO.readFile "bf/helloworld.bf" >>= \file -> defaultMain [ 
    bgroup "parseBrainheck" [ 
        bench "helloworld.bf" $ whnf (parseBrainheck "bf/helloworld.bf") file ] 
    ]
