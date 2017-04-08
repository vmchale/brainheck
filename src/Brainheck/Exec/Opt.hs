module Brainheck.Exec.Opt where

import Options.Applicative
import Brainheck
import qualified Data.Text.IO as TIO
import Data.Monoid
import Control.Monad.State.Lazy

-- TODO move this to app/Main.hs

data Program = Program { filepath :: FilePath }

program :: Parser Program
program = Program
    <$> argument str
    ( metavar "FILE"
    <> help "Brainfuck file" )

exec :: IO ()
exec = runFile =<< execParser opts
  where
    opts = info (program <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

runFile :: Program -> IO ()
runFile program = do
    file <- TIO.readFile (filepath program)
    let parsed = either (error . show) id $ parseBrainheck file
    fst <$> run parsed 
