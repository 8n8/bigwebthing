{-# LANGUAGE OverloadedStrings #-}

module Main where

-- So this is a little programming language, because I find all the
-- ones I have really annoying. The main annoyances are:
-- 
-- 1. It's hard to debug concurrent programs, but you often need concurrency.
--
-- 2. Dynamic languages like Python are too buggy and have rubbish concurrency. Static languages like Haskell and Go are too restrictive and unexpressive.
--
-- 3. I just want to write pure functions, and let the runtime do all the IO and concurrency
--
-- 4. Go isn't immutable
--
-- 5. Haskell is too weird and complicated. I still hate monads.


import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Data.Text.Encoding (decodeUtf8')
import System.Environment (getArgs)
import Data.Void (Void)
import qualified Data.Text.IO as Tio


data Data
    = StringA T.Text
    | FunctionA [Data] Data
    | FunctionCallA [Data]
    | MapA [(Data, Data)]
    | LookupA Data


main :: IO ()
main =
    do
    args <- getArgs
    case args of
        [path] ->
            do
            raw <- B.readFile path
            case parseData raw of
                Left err ->
                    Tio.putStrLn err

                Right data_ ->
                    do
                    eitherChecked <- typeCheck data_
                    case eitherChecked of
                        Left err ->
                            Tio.putStrLn err

                        Right haskell ->
                            put haskell


put :: Haskell -> IO ()
put haskell =
    putStrLn $ show haskell


parseData :: B.ByteString -> Either T.Text Data
parseData raw =
    case decodeUtf8' raw of
    Left err ->
        Left $ T.pack $ show err

    Right text ->
        let
        state = T.foldl' dataP initState text
        in
        case dataS state of
        Nothing ->
            Left $ errorS state

        Just d ->
            Right d


data Haskell
    = Haskell
    deriving Show


typeCheck :: Data -> IO (Either T.Text Haskell)
typeCheck =
    undefined


initState :: P
initState =
    P
    { errorS = ""
    , dataS = Nothing
    , stringS = Nothing
    }


data P
    = P
    { errorS :: T.Text
    , dataS :: Maybe Data
    , constructingS :: Maybe Constructing
    }


map ((key, value), (key, value), (key, value))
data Constructing
    = StringC T.Text
    | FunctionC [Constructing] (Maybe Constructing)
    | FunctionCallC [Constructing]
    | MapA [(


dataP :: P -> Char -> P
dataP p ch =
    case ch of
    '"' ->
        case stringS p of
        Nothing ->
            p { stringS = Just "" }

        Just s ->
            p { stringS = Just $ T.snoc s ch }

    
