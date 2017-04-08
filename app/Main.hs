module Main where

import Brainheck
import Options.Applicative
import qualified Data.Text.IO as TIO
import Data.Monoid

data Program = Program { filepath :: FilePath }

program :: Parser Program
program = Program <$> argument str (metavar "FILE" <> help "Brainfuck file")

main :: IO ()
main = runFile =<< execParser (info (program <**> helper) (fullDesc
     <> progDesc "Brainh*ck - an interpreter"
     <> header "brainheck - a brainfuck intrepreter written in haskell and supporting utf-8")) -- TODO variable array size?

runFile :: Program -> IO ()
runFile program = either (error . show) id . (parseBrainheck (filepath program)) <$> TIO.readFile (filepath program) >>= run
