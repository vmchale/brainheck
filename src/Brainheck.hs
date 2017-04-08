{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Brainheck
    ( exec
    ) where

import qualified Data.Vector as V
import Control.Monad.State.Lazy
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Primitive
import Control.Lens
import Data.Vector.Lens
import qualified Data.Map as M
import Options.Applicative

-- philosophizing with a hammer // catastrophe of abstraction

type St a = StateT IndexArr IO a
type Array = V.Vector Int -- TODO use mutable vector, e.g. V.MVector (PrimState IO) Int
type IndexArr = (Array, Int)

data Syntax a = Loop (Syntax a)
              | Seq [Syntax a]
              | Token Char deriving (Show)

data Program = Program { filepath :: FilePath }

makeBaseFunctor ''Syntax

filterChar :: T.Text -> T.Text
filterChar = T.filter (`elem` "[]+-.,<>")

initial :: IndexArr
initial = (V.replicate 30000 0, 0)

check :: St Bool
check = do
    (arr,i) <- get
    pure . (==0) . (V.! i) $ arr

displayChar :: St ()
displayChar = do
    (arr,i) <- get
    liftIO . putChar . toEnum . (V.! i) $ arr

readChar :: St ()
readChar = do
    (_,i) <- get
    char <- liftIO . (fmap fromEnum) $ getChar
    modifyByIndex i (const char)

modifyIndex :: (Int -> Int) -> St ()
modifyIndex = modify . (over _2)

bumpIndex :: St ()
bumpIndex = modifyIndex (+1)

shrinkIndex :: St ()
shrinkIndex = modifyIndex (subtract 1)

modifyByIndex :: Int -> (Int -> Int) -> St ()
modifyByIndex i = ((_1 . sliced i 1) %%=) . (pure .) . fmap

modifyVal :: (Int -> Int) -> St ()
modifyVal f = do
    (_,i) <- get
    modifyByIndex i f

bumpVal :: St ()
bumpVal = modifyVal (+1)

shrinkVal :: St ()
shrinkVal = modifyVal (subtract 1)

toAction :: Char -> St ()
toAction = maybe (error "unexpected token") id .  flip M.lookup keys
    where keys = M.fromList [ ('.', displayChar)
                            , (',', readChar)
                            , ('+', bumpVal)
                            , ('-', shrinkVal)
                            , ('>', bumpIndex)
                            , ('<', shrinkIndex)
                            ]

brainheck :: Parser (Syntax Char)
brainheck = Seq <$> many (action <|> loop)
    where loop = Loop <$> between (char '[') (char ']') brainheck
          action = Seq . (map Token) <$> (some . oneOf) "+-.,<>"

-- coalgebra :: T.Text -> Base (Syntax Char) T.Text

algebra :: Base (Syntax Char) (St ()) -> St ()
algebra l@(LoopF x) = do
    bool <- check
    if bool then pure () else x >> algebra l 
algebra (TokenF x) = toAction x
algebra (SeqF x) = foldr (>>) (pure ()) x

run :: Syntax Char -> St ()
run = cata algebra

parser :: T.Text -> Either (ParseError (Token T.Text) Dec) (Syntax Char)
parser = (parse (brainheck) "") . filterChar

program :: Parser Program
program = Program
    <$> argument str
    ( metavar "FILE"
    <> help "Brainfuck file" )

main :: IO ()
main = runFile =<< execParser opts
  where
    opts = info (program <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

runFile :: Program -> IO ()
runFile program = do
    file <- TIO.readFile (filepath) program
    let parsed = either (error . show) id $ parser file
    fst <$> runStateT (run parsed) initial
