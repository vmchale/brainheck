module Main where

import Criterion.Main
import Brainheck
import qualified Data.Text.IO as TIO

main = TIO.readFile "bf/helloworld.bf" >>= \file -> defaultMain [ 
    bgroup "parseBrainheck" [ 
        bench "helloworld.bf" $ whnf (parseBrainheck "bf/helloworld.bf") file ] 
    ]
