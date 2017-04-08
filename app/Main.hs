module Main where

import Brainheck
import Options.Applicative
import qualified Data.Text.IO as TIO
import Data.Monoid

program :: Parser FilePath
program = argument str (metavar "FILE" <> help "Brainfuck file")

main :: IO ()
main = let runFile filepath = either (error . show) id . (parseBrainheck filepath) <$> TIO.readFile filepath >>= run in
    runFile =<< execParser (info (program <**> helper) (fullDesc
        <> progDesc "Brainh*ck - an interpreter"
        <> header "brainheck - a brainfuck intrepreter written in haskell and supporting utf-8"))
