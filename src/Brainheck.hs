{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Brainheck
    ( run
    , parseBrainheck
    ) where

import qualified Data.Vector as V
import Control.Monad.State.Lazy
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Text as T
import Control.Monad.Primitive
import Control.Lens
import Data.Vector.Lens
import qualified Data.Map as M

type St a = StateT IndexArr IO a
type IndexArr = (V.Vector Int, Int)
-- TODO use mutable vector, e.g. V.MVector (PrimState IO) Int

data Syntax a = Loop (Syntax a)
              | Seq [Syntax a]
              | Token Char deriving (Show)

makeBaseFunctor ''Syntax

initial :: IndexArr
initial = (V.replicate 30000 0, 0)

check :: St Bool
check = get >>= (\(arr,i) -> pure . (==0) . (V.! i) $ arr)

displayChar :: St ()
displayChar = get >>= (\(arr,i) -> liftIO . putChar . toEnum . (V.! i) $ arr)

readChar :: St ()
readChar = get >>= (\(_,i) -> modifyByIndex i . const =<< (liftIO . (fmap fromEnum)) getChar)

modifyState lens = (lens %%=) . (pure .)

modifyByIndex :: Int -> (Int -> Int) -> St ()
modifyByIndex i = modifyState (_1 . sliced i 1) . fmap

modifyVal :: (Int -> Int) -> St ()
modifyVal f = flip modifyByIndex f . snd =<< get 

toAction :: Char -> St ()
toAction = maybe (error mempty) id . flip M.lookup keys
    where keys = M.fromList [ ('.', displayChar)
                            , (',', readChar)
                            , ('+', modifyVal (+1))
                            , ('-', modifyVal (subtract 1))
                            , ('>', modifyState _2 (+1))
                            , ('<', modifyState _2 (subtract 1))
                            ]

brainheck :: Parser (Syntax Char)
brainheck = Seq <$> many (action <|> loop)
    where loop = Loop <$> between (char '[') (char ']') brainheck
          action = Seq . (fmap Token) <$> (some . oneOf) "+-.,<>"

algebra :: Base (Syntax Char) (St ()) -> St ()
algebra l@(LoopF x) = check >>= (\bool -> if bool then pure () else x >> algebra l)
algebra (TokenF x) = toAction x
algebra (SeqF x) = foldr (>>) (pure ()) x

run :: (Syntax Char) -> IO ()
run parsed = fst <$> runStateT (cata algebra parsed) initial

parseBrainheck :: T.Text -> Either (ParseError (Token T.Text) Dec) (Syntax Char)
parseBrainheck = (parse (brainheck) "") . (T.filter (`elem` "[]+-.,<>"))
