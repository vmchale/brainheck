{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Module with parser etc.
module Brainheck
    ( run
    , parseBrainheck
    -- * Types
    , Syntax (..)
    ) where

import           Control.Lens             hiding (lens)
import           Control.Monad.State.Lazy
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import qualified Data.Map                 as M
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Data.Vector.Lens
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type St a = StateT IndexArr IO a
type IndexArr = (V.Vector Int, Int)
type Parser = Parsec Void T.Text

-- | Syntax tree for brainfuck
data Syntax a = Loop (Syntax a)
              | Seq [Syntax a]
              | Token a deriving (Show)

makeBaseFunctor ''Syntax

-- | Map a char to its action in the `St` monad
toAction :: Char -> St ()
toAction = fromMaybe (error mempty) . flip M.lookup keys
    where modifyVal f = flip modifyByIndex f . snd =<< get
          modifyByIndex i = modifyState (_1 . sliced i 1 . forced) . fmap
          modifyState lens = (lens %%=) . (pure .)
          readChar = get >>= (\(_,i) -> modifyByIndex i . const =<< (liftIO . fmap fromEnum) getChar)
          displayChar = get >>= (\(arr,i) -> liftIO . putChar . toEnum . (V.! i) $ arr)
          keys = M.fromList [ ('.', displayChar)
                            , (',', readChar)
                            , ('+', modifyVal (+1))
                            , ('-', modifyVal (subtract 1))
                            , ('>', modifyState _2 (+1))
                            , ('<', modifyState _2 (subtract 1)) ]

-- | Parse to syntax tree
brainheck :: Parser (Syntax Char)
brainheck = Seq <$> many (Seq . fmap Token <$> (some . oneOf) "+-.,<>"
    <|> Loop <$> between (char '[') (char ']') brainheck)

algebra :: Base (Syntax Char) (St ()) -> St ()
algebra (TokenF x) = toAction x
algebra (SeqF x) = foldr (>>) (pure ()) x
algebra l@(LoopF x) = check >>= (\bool -> if bool then pure () else x >> algebra l)
    where check = get >>= (\(arr,i) -> pure . (==0) . (V.! i) $ arr)

-- | Evaluate syntax tree
run :: Syntax Char -> IO ()
run parsed = fst <$> runStateT (cata algebra parsed) (V.replicate 30000 0, 0)

-- | Parse and return an error or a syntax tree
parseBrainheck :: FilePath -> T.Text -> Either (ParseError (Token T.Text) Void) (Syntax Char)
parseBrainheck filepath = parse brainheck filepath . T.filter (`elem` "[]+-.,<>")
