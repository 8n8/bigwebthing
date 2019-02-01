{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import qualified Control.Concurrent.STM as Stm
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as Ds
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

type Namespace = M.Map T.Text Expression

data Expression where
    I :: Integer -> Expression
    F :: Float -> Expression
    S :: T.Text -> Expression
    C :: Char -> Expression
    T1 :: (a) -> Expression
    T2 :: (a, b) -> Expression
    T3 :: (a, b, c) -> Expression
    T4 :: (a, b, c, d) -> Expression
    T5 :: (a, b, c, d, e) -> Expression
    T6 :: (a, b, c, d, e, f) -> Expression
    List :: [a] -> Expression
    Map :: M.Map a b -> Expression
    Set :: Ds.Set a -> Expression
    MutVar :: Stm.TVar a -> Expression
    Queue :: Stm.TQueue a -> Expression
    Atom :: T.Text -> Expression
    Function :: [T.Text] -> Namespace -> Expression
    Evaluate :: T.Text -> [Expression] -> Expression

parseFloat :: Parser Expression
parseFloat = F . read <$> (parseDigits <|> parseLongFloat)

parseLongFloat :: Parser String
parseLongFloat = try $ do
    beforePoint <- try $ some digitChar
    afterPoint <- parseDecimal
    return $ beforePoint ++ '.':afterPoint

parseDecimal :: Parser String
parseDecimal = char '.' >> parseDigits

parseInt :: Parser Expression
parseInt = fmap (I . read) parseDigits

parseDigits :: Parser String
parseDigits = do
    int <- try $ some digitChar
    parseOneWhiteSpace
    return int 

parseOneWhiteSpace :: Parser ()
parseOneWhiteSpace = void (try $ char ' ') <|> void (try newline)

main :: IO ()
main = do
  putStrLn "hello world"

