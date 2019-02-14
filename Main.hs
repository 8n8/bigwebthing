{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

-- import qualified Control.Concurrent.STM as Stm
import Control.Monad
import qualified Data.ByteString as B
-- import qualified Data.Map as M
-- import qualified Data.Set as Ds
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void T.Text

type Namespace = [(Maybe Name, Expression, SourcePos)]

newtype NameStr = NameStr T.Text deriving (Eq, Ord, Show)
newtype TypeStr = TypeStr T.Text deriving (Eq, Ord, Show)

type Name = (NameStr, Type)

data Type where
    StrT :: Type
    IntT :: Type
    FloatT :: Type
    IOFuncT :: [Type] -> Type -> Type
    NilT :: Type

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
        :: [Name] -- The input arguments.
        -> Namespace -- Local name bindings.
        -> Maybe Expression -- The return expression.
        -> Expression
    Evaluate :: T.Text -> [Expression] -> Expression
    deriving (Show)

parseSingleString :: Parser Expression
parseSingleString = dbg "parseSingleString" $
  do
    _ <- char '"'
    str <- some parseStrChar
    _ <- char '"'
    return $ S $ T.pack str
    
parseStrChar :: Parser Char
parseStrChar =
  choice
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
    lookAhead $ parseOneWhiteSpace <|> void (char ')')
    return int

parseIoFunc :: Int -> Parser (Name, Expression)
parseIoFunc level = dbg "parseIoFunc" $
  do
    try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        void $ string "dio "
    (name, arglist, funcType) <- parseFuncDeclaration
    _ <- newline
    let i = level + 1
    exprs <- many $ parseElement i
    returnVal <- fmap Just (parseReturnExpr i) <|> return Nothing
    dbg "trailingWSParser" $ if level == 0 then
        dbg "level0" $ try (lookAhead $ (void newline) >> eof) <|> (try $ void (count 3 newline) >> notFollowedBy (space >> eof)) <|> return ()
    else dbg "level1ormore" $ try (lookAhead eof) <|> void newline
    return ( (name, funcType)
           , IOFunction arglist exprs returnVal
           )


parseMaybeIoFunc :: Int -> Parser (Maybe Name, Expression)
parseMaybeIoFunc level = dbg "parseMaybeIoFunc" $
  do
    (name, expr) <- parseIoFunc level
    return (Just name, expr)
        
parseFuncDeclaration :: Parser (NameStr, [NameStr], Type)
parseFuncDeclaration = dbg "parseFuncDeclaration" $
  do
    name <- try parseName
    _ <- char '('
    args <- many parseFuncArg  
    _ <- char ')'
    returnType <- parseReturnType
    let funcType = concat (map snd args) ++ returnType
    return (NameStr name, map fst args, funcType)

parseReturnExpr :: Int -> Parser Expression
parseReturnExpr level = dbg "parseReturnExpr" $
  do
    try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        void $ string "return "
    parseExpression

parseBinding :: Int -> Parser (Maybe Name, Expression)
parseBinding level = dbg "parseBinding" $
  do
    nameStr <- try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        ns <- parseName
        void $ char ' '
        return ns
    typeStr <- parseName
    let name = (NameStr nameStr, [Just (TypeStr typeStr)])
    _ <- char ' '
    _ <- char '='
    _ <- char ' '
    value <- parseInt <|> parseFloat <|> parseSingleString
    parseBindingWhitespace level
    return (Just name, value)

parseBindingWhitespace :: Int -> Parser ()
parseBindingWhitespace level = dbg "parseBindingWhitespace" $
    if level == 0 then choice
        [ dbg "bw1" $ try (lookAhead $ void newline >> eof) 
        , dbg "bw2" $ try $ newline >> (lookAhead $ void parseName)
        , dbg "bw3" $ void (count 3 newline) >> lookForFuncDec
        ]
    else dbg "bw4" $ (try $ lookAhead eof) <|>
                     (try (lookAhead (space >> eof)) <|>
                     void newline)

lookForFuncDec :: Parser ()
lookForFuncDec = void $ try $ lookAhead $ choice
    [ string "def ", string "dio ", string "dtm " ]

parseExpression :: Parser Expression
parseExpression = dbg "parseExpression" $
  choice
    [ parseInt
    , parseFloat
    , parseSingleString
    , parseEvaluate
    ]

parseEvaluate :: Parser Expression
parseEvaluate = dbg "parseEvaluate" $
    parseVarEval <|> parseFuncEval

parseVarEval :: Parser Expression
parseVarEval = dbg "parseVarEval" $
  try $ do
    name <- parseName
    lookAhead $ void (char ' ') <|> void (char '\n')
    return $ Evaluate name []

parseFuncEval :: Parser Expression
parseFuncEval = dbg "parseFuncEval" $
  try $ do
    name <- parseName
    _ <- char '('
    inputs <- many parseFuncInput
    _ <- char ')'
    return $ Evaluate name inputs
    
parseFuncInput :: Parser Expression
parseFuncInput = dbg "parseFuncInput" $
  do
    expr <- parseExpression
    void (string ", ") <|> lookAhead (void $ char ')')
    return expr

parseReturnType :: Parser Type
parseReturnType = dbg "parseReturnType" $
  choice
    [ void (char ':') >> return [Nothing]
    , parseExplicitReturnType
    ]

parseExplicitReturnType :: Parser Type
parseExplicitReturnType = dbg "parseExplicitReturnType" $
  do
    _ <- string " -> "
    typeStr <- parseName
    _ <- char ':'
    return $ [Just (TypeStr typeStr)]

parseFuncArg :: Parser Name
parseFuncArg = dbg "parseFuncArg" $
  do
    arg <- parseNameAndType
    void (string ", ") <|> lookAhead (void $ char ')')
    return arg

parseNameAndType :: Parser Name
parseNameAndType = dbg "parseNameAndType" $
  do
    name <- parseName
    _ <- char ' '
    typeStr <- parseName
    return (NameStr name, [Just (TypeStr typeStr)])

parseName :: Parser T.Text
parseName = dbg "parseName" $
  do
    name <- parseFuncChars
    let txtName = T.pack name
    if onlyNums name then
        fail "Names can't only have numbers in them."
    else
        if txtName `elem` reservedNames then
            fail "Name is reserved."
        else return txtName
    
reservedNames :: [T.Text]
reservedNames = ["def", "dio", "interface", "return"]

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
parseNIndents = dbg "parseNIndents" $
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

parseUnboundFuncEval :: Int -> Parser (Maybe Name, Expression)
parseUnboundFuncEval level = dbg "parseUnboundFuncEval" $
  try $ do
    indents <- parseNIndents
    when (indents /= level) (fail "Bad indentation.")
    eval <- parseFuncEval
    parseBindingWhitespace level
    return (Nothing, eval)

parseElement :: Int -> Parser (Maybe Name, Expression, SourcePos)
parseElement level = do
    pos <- getSourcePos
    (name, expr) <- choice
        [ parseMaybeIoFunc level
        , parseBinding level
        , parseUnboundFuncEval level
        ]
    return (name, expr, pos)

builtIns :: [Name]
builtIns =
    [ (NameStr "print", [Just (TypeStr "str"), Nothing])
    ]

nameMatch :: T.Text -> (Maybe Name, Expression, SourcePos) -> Bool
nameMatch _ (Nothing, _, _) = False
nameMatch lookingFor (Just (NameStr name, _), _, _) =
    lookingFor == name

lookupNew
    :: T.Text
    -> Namespace
    -> [(Maybe Name, Expression, SourcePos)]
lookupNew name namespace = filter (nameMatch name) namespace

lookupBuiltIn :: T.Text -> [Name]
lookupBuiltIn name = filter (\(NameStr n, _) -> n == name) builtIns 

typecheck
    :: (Maybe Name, Expression, SourcePos)
    -> Namespace
    -> Maybe T.Text
typecheck element namespace = case element of
    (Nothing, Evaluate name args, sourcePos) ->
        case (lookupNew name namespace, lookupBuiltIn name) of
            ([], []) -> Just $ T.concat
                [ T.pack $ sourcePosPretty sourcePos
                , "Name \""
                , name
                , "\" could not be found."
                ]
            ([(Nothing, _, _)], []) ->
                Just $ "This error should never happen and is a " <>
                       "bug in the compiler."
            ([(Just (NameStr name, types), expr, pos)], []) ->
                undefined 
                
extractNewNameTypes :: [(Maybe Name, Expression, SourcePos)] -> [Type]
extractNewName names =
    let
       getType (Just (_, t), _, _) = Just t
       getType (Nothing, _, _) = Nothing
    in
       justs $ map getType names

getNamedTypes :: T.Text -> Namespace -> [Type]
getNamedTypes name ns =
    extractNewNameTypes (lookUpNew name ns) ++
        map snd lookupBuiltIn name

getExprType :: Expression -> Type
getExprType expr = case expr of
    I _ -> IntT
    F _ -> FloatT
    S _ -> StrT
    IOFunction args _ retExp ->
        IOFuncT (map snd args) (getExprType retExp)
    Evaluate nameStr args ->
        let
            -- candidateTypes = getNamedTypes name
            argTypes = map getExprType args

matchingTypes :: [Type] -> [Type] -> Maybe Type
matchingTypes

getReturnType :: T.Text -> [Type] -> Namespace -> Maybe Type
getReturnType nameStr argTypes namespace =
    let
        candidates = getNamedTypes name namespace
    in
        case filter ((argTypes ==) . init) 

parseNamespace :: Parser Namespace
parseNamespace = dbg "parseNamespace" $
  do
    namespace <- many $ parseElement 0
    _ <- newline
    eof
    return namespace

main :: IO ()
main = do
    filepath <- fmap head getArgs
    filebytes <- B.readFile filepath 
    let filetext = decodeUtf8 filebytes
    case parse parseNamespace filepath filetext of
        Left err -> putStrLn (errorBundlePretty err)
        Right a -> print a
