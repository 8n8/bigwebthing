{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

-- import qualified Control.Concurrent.STM as Stm
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Map as M
-- import qualified Data.Set as Ds
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void T.Text

type Namespace = M.Map Name Expression

newtype NameStr = NameStr T.Text deriving (Eq, Ord, Show)
newtype TypeStr = TypeStr T.Text deriving (Eq, Ord, Show)

type Name = (NameStr, Type)

type Type = [Maybe TypeStr]

data Expression where
    I :: Integer -> Expression
    F :: Float -> Expression
    S :: T.Text -> Expression
    -- C :: Char -> Expression
    -- T1 :: (a) -> Expression
    -- T2 :: (a, b) -> Expression
    -- T3 :: (a, b, c) -> Expression
    -- T4 :: (a, b, c, d) -> Expression
    -- T5 :: (a, b, c, d, e) -> Expression
    -- T6 :: (a, b, c, d, e, f) -> Expression
    -- List :: [a] -> Expression
    -- Map :: M.Map a b -> Expression
    -- Set :: Ds.Set a -> Expression
    -- MutVar :: Stm.TVar a -> Expression
    -- Queue :: Stm.TQueue a -> Expression
    -- Atom :: T.Text -> Expression
    -- LetIn :: Namespace -> Expression
    IOFunction
        :: [NameStr] -- The input arguments.
        -> [(Maybe Name, Expression)] -- Local name bindings.
        -> Expression -- The return expression.
        -> Expression
    Evaluate :: T.Text -> [Expression] -> Expression
    deriving (Show)

parseSingleString :: Parser Expression
parseSingleString = do
    _ <- char '"'
    str <- some parseStrChar
    _ <- char '"'
    return $ S $ T.pack str
    
parseStrChar :: Parser Char
parseStrChar = choice
    [ noneOf ("\"\\" :: String)
    , parseLiteralQuote
    , parseLiteralBackslash
    ]

parseLiteralQuote :: Parser Char
parseLiteralQuote = void (string "\\\"") >> return '"'

parseLiteralBackslash :: Parser Char
parseLiteralBackslash = void (string "\\\\") >> return '\\'

parseFloat :: Parser Expression
parseFloat = F . read <$> (parseLongFloat <|> parseDigits)

parseLongFloat :: Parser String
parseLongFloat = try $ do
    beforePoint <- some digitChar
    afterPoint <- parseDecimal
    return $ beforePoint ++ '.':afterPoint

parseDecimal :: Parser String
parseDecimal = char '.' >> parseDigits

parseInt :: Parser Expression
parseInt = try $ fmap (I . read) parseDigits

parseDigits :: Parser String
parseDigits = do
    int <- try $ some digitChar
    lookAhead $ parseOneWhiteSpace
    return int

parseIoFunc :: Int -> Parser (Name, Expression)
parseIoFunc level = try $ do
    indents <- parseNIndents
    when (indents /= level) (fail "Bad indentation.")
    _ <- string "dio "
    (name, arglist, funcType) <- parseFuncDeclaration
    _ <- newline
    let i = level + 1
    exprs <- many $ parseBinding i <|> parseMaybeIoFunc i
    returnVal <- parseReturnExpr i
    return ( (name, funcType)
           , IOFunction arglist exprs returnVal
           )

parseMaybeIoFunc :: Int -> Parser (Maybe Name, Expression)
parseMaybeIoFunc level = try $ do
    (name, expr) <- parseIoFunc level
    return (Just name, expr)
        
parseFuncDeclaration :: Parser (NameStr, [NameStr], Type)
parseFuncDeclaration = do
    name <- parseName
    _ <- char '('
    args <- many parseFuncArg  
    _ <- char ')'
    returnType <- parseReturnType
    let funcType = concat (map snd args) ++ returnType
    return (NameStr name, map fst args, funcType)

parseReturnExpr :: Int -> Parser Expression
parseReturnExpr level = do
    indents <- parseNIndents
    when (indents /= level) (fail "Bad indentation.")
    _ <- string "return "
    parseExpression

parseBinding :: Int -> Parser (Maybe Name, Expression)
parseBinding level = do
    indents <- parseNIndents
    when (indents /= level) (fail "Bad indentation.")
    name <- (Just <$> parseNameAndEquals) <|> return Nothing
    value <- parseInt <|> parseFloat <|> parseSingleString
    _ <- newline
    return (name, value)

parseExpression :: Parser Expression
parseExpression = choice
    [ parseInt
    , parseFloat
    , parseSingleString
    , parseEvaluate
    ]

parseEvaluate :: Parser Expression
parseEvaluate = parseVarEval <|> parseFuncEval

parseVarEval :: Parser Expression
parseVarEval = do
    name <- parseName
    lookAhead $ void (char ' ') <|> void (char '\n')
    return $ Evaluate name []

parseFuncEval :: Parser Expression
parseFuncEval = do
    name <- parseName
    _ <- char '('
    inputs <- many parseFuncInput
    _ <- char ')'
    return $ Evaluate name inputs
    
parseFuncInput :: Parser Expression
parseFuncInput = do
    expr <- parseExpression
    void (string ", ") <|> lookAhead (void $ char ')')
    return expr
    
parseNameAndEquals :: Parser Name
parseNameAndEquals = try $ do
    name <- parseNameAndType
    _ <- string " = "
    return name

parseReturnType :: Parser Type
parseReturnType = choice
    [ void (char ':') >> return [Nothing]
    , parseExplicitReturnType
    ]

parseExplicitReturnType :: Parser Type
parseExplicitReturnType = do
    _ <- string " -> "
    typeStr <- parseName
    _ <- char ':'
    return $ [Just (TypeStr typeStr)]

parseFuncArg :: Parser Name
parseFuncArg = do
    arg <- parseNameAndType
    void (string ", ") <|> lookAhead (void $ char ')')
    return arg

parseNameAndType :: Parser Name
parseNameAndType = do
    name <- parseName
    _ <- char ' '
    typeStr <- parseName
    return (NameStr name, [Just (TypeStr typeStr)])

parseName :: Parser T.Text
parseName = do
    name <- parseFuncChars
    let txtName = T.pack name
    if onlyNums name then
        fail "Function names can't only have numbers in them."
    else
        if txtName `elem` reservedNames then
            fail "Name is reserved."
        else return txtName
    
reservedNames :: [T.Text]
reservedNames = ["def", "dio", "interface"]

onlyNums :: String -> Bool
onlyNums str = all (`elem` ("0123456789" :: String)) str

parseFuncChars :: Parser String
parseFuncChars = try $ some $ oneOf fnameChars
   
fnameChars :: String
fnameChars = "abcdefghijklmnopqrstuvwxyz\
             \ABCDEFGHIJKLMNOPQRSTUVWXYZ\
             \0123456789\
             \!£$&_?;'@#~"

parseNIndents :: Parser Int
parseNIndents =
    let
        f :: Int -> Parser Int
        f n = (parseOneIndent >> f (n + 1)) <|> return n
    in f 0
    
parseOneIndent :: Parser ()
parseOneIndent =
    let oneSpace = void $ char ' '
    in oneSpace >> oneSpace >> oneSpace >> oneSpace

parseOneWhiteSpace :: Parser ()
parseOneWhiteSpace = void (try $ char ' ') <|> void (try newline)

parseTopLevelBinding :: Parser (Name, Expression)
parseTopLevelBinding = do
    (maybeName, value) <- parseBinding 0
    case maybeName of
        Nothing -> fail "Top-level bindings must have names."
        Just name -> return (name, value)

parseOneTopLevel :: Parser (Name, Expression)
parseOneTopLevel = do
    result <- parseTopLevelBinding <|> parseIoFunc 0
    _ <- many newline
    return result

parseNamespace :: Parser Namespace
parseNamespace = do
    namespace <- some parseOneTopLevel
    return $ M.fromList namespace

main :: IO ()
main = do
    filepath <- fmap head getArgs
    filebytes <- B.readFile filepath 
    let filetext = decodeUtf8 filebytes
    case parse parseNamespace filepath filetext of
        Left err -> putStrLn (errorBundlePretty err)
        Right a -> print a
