module Main where

import           Brainheck
import qualified Data.Text.IO        as TIO
import           Data.Version
import           Options.Applicative
import           Paths_brainheck

program :: Parser FilePath
program = argument str (metavar "FILE" <> help "Brainfuck file")

main :: IO ()
main = let runFile filepath = either (error . show) id . (parseBrainheck filepath) <$> TIO.readFile filepath >>= run in
    runFile =<< execParser (info (program <**> helper <**> versionInfo) (fullDesc
        <> progDesc "Brainh*ck - an interpreter"
        <> header "brainheck - a brainfuck intrepreter written in haskell and supporting utf-8"))
    where versionInfo = infoOption ("brainheck version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
