{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

-- import qualified Control.Concurrent.STM as Stm
import Control.Monad
import qualified Data.ByteString as B
-- import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void T.Text

type Namespace = [NsElement]

data NsElement
    = NameType NameStr Type Expression SourcePos
    | TypeOnly Type Expression SourcePos
    | NoNameType Expression SourcePos
    deriving Show

newtype NameStr = NameStr T.Text deriving (Eq, Ord, Show)
newtype TypeStr = TypeStr T.Text deriving (Eq, Ord, Show)

type Name = (NameStr, Type)

newtype Type = Type Expression deriving Show

-- data Type
--     = NormalT T.Text
--     | FIo [Type] (Maybe Type)
--     deriving (Eq, Show)

-- type Type = ([TypeStr], [TypeStr])

type Expression = [Value]

data Value
    = TypeT
    | IoFunT [Expression] (Maybe Expression)
    | IoFun [Name] Namespace (Maybe Expression)
    | Evaluate T.Text [Expression]
    deriving (Show)

-- data Expression where
--     TypeT :: Expression
--     IntOne :: Integer -> Expression
--     IntSet :: S.Set Integer -> Expression
--     IntAll :: Expression
--     FloatOne :: Float -> Expression
--     FloatSet :: S.Set Float -> Expression
--     FloatAll :: Expression
--     StrOne :: T.Text -> Expression
--     StrSet :: S.Set T.Text -> Expression
--     StrAll :: Expression
--     IoFT :: [Expression] -> Maybe Expression -> Expression
--     -- C :: Char -> Expression
--     -- T1 :: (a) -> Expression
--     -- T2 :: (a, b) -> Expression
--     -- T3 :: (a, b, c) -> Expression
--     -- T4 :: (a, b, c, d) -> Expression
--     -- T5 :: (a, b, c, d, e) -> Expression
--     -- T6 :: (a, b, c, d, e, f) -> Expression
--     -- List :: [a] -> Expression
--     -- Map :: M.Map a b -> Expression
--     -- Set :: Ds.Set a -> Expression
--     -- MutVar :: Stm.TVar a -> Expression
--     -- Queue :: Stm.TQueue a -> Expression
--     -- Atom :: T.Text -> Expression
--     -- LetIn :: Namespace -> Expression
--     IOFunction
--         :: [Name] -- The input arguments.
--         -> Namespace -- Local name bindings.
--         -> Maybe Expression -- The return expression.
--         -> Expression
--     Evaluate :: T.Text -> [Expression] -> Expression
--     deriving (Show)

numArgs :: Expression -> Int
numArgs expr = case expr of
    IoFun args _ _ -> length args
    _ -> 0

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

parseIoFunc :: Int -> Parser (NameStr, Type, Expression)
parseIoFunc level = dbg "parseIoFunc" $
  do
    try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        void $ string "dio "
    (name, arglist, ins, out) <- parseFuncDeclaration
    _ <- newline
    let i = level + 1
    exprs <- many $ parseElement i
    returnVal <- fmap Just (parseReturnExpr i) <|> return Nothing
    dbg "trailingWSParser" $ if level == 0 then
        dbg "level0" $ try (lookAhead $ (void newline) >> eof) <|> (try $ void (count 3 newline) >> notFollowedBy (space >> eof)) <|> return ()
    else dbg "level1ormore" $ try (lookAhead eof) <|> void newline
    return ( name
           , FIo ins out
           , IOFunction (zip arglist ins) exprs returnVal
           )

parseMaybeIoFunc
    :: Int -> Parser (Maybe NameStr, Maybe Type, Expression)
parseMaybeIoFunc level = dbg "parseMaybeIoFunc" $
  do
    (name, typeT, expr) <- parseIoFunc level
    return (Just name, Just typeT, expr)
        
parseFuncDeclaration :: Parser (NameStr, [NameStr], [Type], Maybe Type)
parseFuncDeclaration = dbg "parseFuncDeclaration" $
  do
    name <- try parseName
    _ <- char '('
    args <- many parseFuncArg  
    _ <- char ')'
    returnType <- parseReturnType
    return (NameStr name, map fst args, map snd args, returnType)

parseReturnExpr :: Int -> Parser Expression
parseReturnExpr level = dbg "parseReturnExpr" $
  do
    try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        void $ string "return "
    parseExpression

parseBinding :: Int -> Parser (Maybe NameStr, Maybe Type, Expression)
parseBinding level = dbg "parseBinding" $
  do
    nameStr <- try $ do
        indents <- parseNIndents
        when (indents /= level) (fail "Bad indentation.")
        ns <- parseName
        void $ char ' '
        return ns
    typeStr <- parseName
    _ <- char ' '
    _ <- char '='
    _ <- char ' '
    value <- parseInt <|> parseFloat <|> parseSingleString
    parseBindingWhitespace level
    return (Just (NameStr nameStr), Just (NormalT typeStr), value)

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

parseReturnType :: Parser (Maybe Type)
parseReturnType = dbg "parseReturnType" $
  choice
    [ void (char ':') >> return Nothing
    , fmap Just parseExplicitReturnType
    ]

parseExplicitReturnType :: Parser Type
parseExplicitReturnType = dbg "parseExplicitReturnType" $
  do
    _ <- string " -> "
    typeStr <- parseName
    _ <- char ':'
    return $ NormalT typeStr

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
    return (NameStr name, Type typeStr)

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

parseUnboundFuncEval
    :: Int -> Parser (Maybe NameStr, Maybe Type, Expression)
parseUnboundFuncEval level = dbg "parseUnboundFuncEval" $
  try $ do
    indents <- parseNIndents
    when (indents /= level) (fail "Bad indentation.")
    eval <- parseFuncEval
    parseBindingWhitespace level
    return (Nothing, Nothing, eval)

parseElement
    :: Int
    -> Parser (Maybe NameStr, Maybe Type, Expression, SourcePos)
parseElement level = do
    pos <- getSourcePos
    (name, typeT, expr) <- choice
        [ parseMaybeIoFunc level
        , parseBinding level
        , parseUnboundFuncEval level
        ]
    return (name, typeT, expr, pos)

builtIns :: [Name]
builtIns =
    [ (NameStr "print", Type (Simple $ IoFunT [[StrAll]] Nothing))
    ]

typeErr :: SourcePos -> T.Text -> T.Text
typeErr pos errMsg = T.concat
    [ T.pack $ sourcePosPretty pos
    , "\n"
    , errMsg
    ]

noPoint :: T.Text -> T.Text
noPoint msg = "There's no point in " <> msg <> " with no name."

takeSafe :: Int -> [a] -> Maybe [a]
takeSafe n ls =
    if n > length ls then
        Nothing
    else Just $ take n ls
   
dropSafe :: Int -> [a] -> Maybe [a]
dropSafe n ls =
    if n > length ls then
        Nothing
    else Just $ drop n ls

-- argsOkOnce :: [Name] -> Type -> Maybe T.Text
-- argsOkOnce args namedType =
--     case takeSafe (length args) namedType of
--         Nothing -> Just "Too many arguments."
--         Just shouldMatch ->
--             if shouldMatch /= args then
--                 Just $ T.concat
--                     [ "Expected argument types:\n "
--                     , show shouldMatch
--                     , ", but got:\n "
--                     , show args
--                     , "." 
--                     ]
--             else Nothing
-- 
-- argsOk :: [Name] -> [Type] -> Either T.Text Type
-- argsOk args candidates =
--     case justs $ map (argsOkOnce args) candidates of
--         [] -> Left "No types match the given arguments."
--         [t] -> Right t
--         matching -> Left $ T.concat 
--             [ "There is more than one matching name: "
--             , T.pack $ show matching
--             ]

lookUpArgType :: [Name] -> T.Text -> [Type]
lookUpArgType args name =
    map snd $ filter ((NameStr name ==) . fst) args

lookupTypes :: T.Text -> Namespace -> [Name] -> [Type]
lookupTypes name ns argSpace = 
    getNamedType name ns ++ lookUpArgType argSpace name

typecheck
    :: Namespace
    -> [Name]
    -> (Maybe NameStr, Maybe Type, Expression, SourcePos)
    -> Maybe T.Text
typecheck namespace argSpace exprInfo = case exprInfo of
    TypeOnly _ expr pos -> case expr of
        Simple _ -> Just $ typeErr pos $ noPoint ""
        IoFun _ _ _ ->
            Just $ "Unnamed functions are not allowed. This is a \
              \bug in the compiler and should not have passed the \
              \parser."
        Evaluate name args ->
            let
                
            in case lookupTypes name namespace argSpace of
                [] -> Just $ typeErr pos $ T.concat
                    [ "Function \""
                    , name
                    , "\" does not exist."
                    ] 
                [[Simple _]] -> Just $ typeErr pos $ T.concat
                    [ "There is no point in a simple type without "
                    , "a name."
                    ]
                [[IoFunT ins _]] -> 
                    let c (e, t) = typecheck namespace argSpace
                         (Nothing, Just t, e, pos)
                    in case justs (map c (zip args ins)) of
                       [] -> Nothing
                       errs -> Just $ T.concat errs 
                _ -> Just $ typeErr pos $ T.concat
                    [ "There were multiple functions with the "
                    , "name \""
                    , name
                    , "\" which is not allowed."
                    ]
    -- (Just _, Nothing, _, pos) -> Just $ typeErr pos "It is not \
    --     \allowed to have a name without an accompanying type \
    --     \declaration."
    -- (Just _, Just (NormalT "int"), I _, _) -> Nothing
    -- (Just _, Just (NormalT "float"), F _, _) -> Nothing
    -- (Just _, Just (NormalT "str"), S _, _) -> Nothing
    -- NameType (NameStr fname) 
    -- (Just (NameStr fname), Just (NormalT _), IOFunction _ _ _, pos) ->
    --     Just $ typeErr pos $ T.concat
    --         [ "Function \""
    --         , fname
    --         , "\" has a simple type, but it should have a function "
    --         , "type."
    --         ]
    NameType _ (IoFunT _ (Just _)) (IoFun args ns Nothing) _ ->
        let checker = typecheck (namespace ++ ns) (args ++ argSpace)
            errs = justs $ map checker ns
        in case errs of
            [] -> Nothing
            _ -> Just $ T.concat errs
    NameType _ (IoFunT _ (Just outputType)) (IoFun args ns (Just retExpr)) pos ->
        let checker = typecheck (namespace ++ ns) (args ++ argSpace)
            errs = justs $ map checker ns
            retErr expectedRetType = checker
                ( Just $ NameStr "return"
                , Just expectedRetType
                , retExpr
                , pos
                )
        in case (errs, retErr outputType) of
            ([], Nothing) -> Nothing
            (_, Nothing) -> Just $ T.concat errs
            (_, Just err) -> Just $ T.concat $ err : errs
    NameType _ ftype (Evaluate name inputs) pos ->
        let nametype = getNamedType name namespace ++
                           lookUpArgType argSpace name
        in case nametype of
            [] -> Just $ typeErr pos $ T.concat
                [ "Could not find name \""
                , name
                , "\"."
                ]
            [[IoFunT inputTypes outputType]] ->
                if length inputTypes /= length inputs then
                    Just $ typeErr pos $ T.concat
                        [ "Function \""
                        , name
                        , "\" takes "
                        , T.pack $ show $ length inputTypes
                        , " arguments, but "
                        , T.pack $ show $ length inputs
                        , " were given."
                        ]
                else
                   let
                     checker expr t = typecheck namespace argSpace 
                           (Nothing, Just t, expr, pos)
                     errs = zipWith checker inputs inputTypes
                   in case (errs, outputType) of
                       ([], Just otype) -> if ftype /= otype then
                           Just $ typeErr pos $ T.concat
                               [ "Function \""
                               , name
                               , "\" returns type \""
                               , T.pack $ show otype
                               , "\", but type \""
                               , T.pack $ show ftype
                               , "\" is required."
                               ]
                            else Nothing
                       _ -> Just $ typeErr pos $
                                T.unlines $ justs errs
            [[Simple _]] ->
                if not (null inputs) then
                    Just $ typeErr pos $ T.concat
                        [ "\""
                        , name
                        , "\" is just a variable, and does not take "
                        , "any arguments."
                        ]
                else Nothing
            _ -> Just $ typeErr pos $ T.concat 
                [ "There is more than one name matching \""
                , name
                , "\", so I can't tell which one to use."
                ]
        
nameMatch :: T.Text -> (Maybe NameStr, Maybe Type, Expression, SourcePos) -> Bool
nameMatch _ (Nothing, _, _, _) = False
nameMatch lookingFor (Just (NameStr name), _, _, _) =
    lookingFor == name

lookupNew
    :: T.Text
    -> Namespace
    -> [(Maybe NameStr, Maybe Type, Expression, SourcePos)]
lookupNew name = filter (nameMatch name)

lookupBuiltIn :: T.Text -> [Name]
lookupBuiltIn name = filter (\(NameStr n, _) -> n == name) builtIns 

extractNewNameTypes :: [(Maybe NameStr, Maybe Type, Expression, SourcePos)] -> [Type]
extractNewNameTypes names =
    let
       getType (_, Just t, _, _) = Just t
       getType (_, Nothing, _, _) = Nothing
    in
       justs $ map getType names

justs :: [Maybe a] -> [a]
justs ms =
    let
        f :: Maybe a -> [a] -> [a]
        f Nothing js = js
        f (Just j) js = j:js
    in foldr f [] ms

getNamedType :: T.Text -> Namespace -> [Expression]
getNamedType name ns =
    extractNewNameTypes (lookupNew name ns) ++
          map snd (lookupBuiltIn name)

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
        Right namespace ->
            case justs $ map (typecheck namespace []) namespace of
                [] -> putStrLn "All the types are fine!"
                errMsgs -> print errMsgs
              
