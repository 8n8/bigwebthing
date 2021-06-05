module Main (main) where


import Prelude
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Exception (try)
import qualified Data.Text.Encoding as E
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.IO
import qualified Data.Map as M


initState =
    State
    { codePathS = Nothing
    }


main :: IO ()
main =
    mainHelp initState Start


mainHelp :: State -> Input -> IO ()
mainHelp state input =
    let
    (state', output) = update state input
    in
    io state' output


data Input
    = Start
    | Args [String]
    | CodeBytes (Either IOError B.ByteString)


data W
    = W
    { startW :: Position
    , valueW :: V
    , endW :: Position
    }
    deriving (Eq, Ord)


data Position
    = CodeP Int
    | RuntimeP
    deriving (Eq, Ord)


data Access a
    = Public a
    | Private a
    deriving (Eq, Ord)


data V
    = Map (M.Map W (Access W))
    | MapLookupInner W W
    | MapLookupOuter W W
    | List [W]
    | StringV T.Text
    | IntV Int
    | ReadFileV W
    | Switch W (M.Map W W)
    | AllInputV
    | AllV
    | AllFunctions
    deriving (Eq, Ord)


data State
    = State
    { codePathS :: Maybe String
    , valueS :: Maybe W
    }


data Output
    = GetArgs
    | ReadCode String
    | Print T.Text
    | Outs [Output]
    | DoNothing


io :: State -> Output -> IO ()
io state output =
    let
    f = mainHelp state
    in
    case output of
    GetArgs ->
        do
        args <- getArgs
        f $ Args args

    ReadCode path ->
        do
        contents <- try $ B.readFile path
        f $ CodeBytes contents

    Print message ->
        Data.Text.IO.putStr message


update :: State -> Input -> (State, Output)
update state input =
    case input of
    Start ->
        (state, GetArgs)

    Args [codePath] ->
        (state, ReadCode codePath)
        
    Args [] ->
        (state, Print $ pretty $ BadDoc NoArgs ArgsDoc)

    Args (_:_:_) ->
        (state, Print $ pretty $ BadDoc TooManyArgs ArgsDoc)

    CodeBytes (Left err) ->
        ( state
        , Print $ pretty $
            case codePathS state of
            Nothing ->
                BadInternal NoCodePath

            Just path ->
                BadDoc (NoCodeFile err path) ArgsDoc
        )

    CodeBytes (Right bytes) ->
        onCodeBytes state bytes


onCodeBytes state bytes =
    case (E.decodeUtf8' bytes, codePathS state) of
    (Left err, Just path) ->
        ( state
        , Print $ pretty $ BadDoc (BadEncoding err path) EncodingDoc
        )

    (Left _, Nothing) ->
        (state, Print $ pretty $ BadInternal NoCodePath)

    (Right decoded, _) ->
        case parse decoded of
        Left err ->
            (state, Print $ pretty $ ParseErrorU err)

        Right w ->
            case evaluate w of
            ErrorE err ->
                (state, Print $ pretty $ SemanticErrorU err)

            EvaluatedE w' ->
                onFullyEvaluated state w'


onFullyEvaluated state w =
    case valueW w of
    Map _ ->
        let
        key =
            W
            { startW = RuntimeP
            , valueW = StringV "action"
            , endW = RuntimeP
            }
        lookup =
            W
            { startW = RuntimeP
            , valueW = MapLookupOuter key w
            , endW = RuntimeP
            }
        in
        case evaluate lookup of
            IoE ioe ->
                (state, Print $ pretty $ BadInternal $ RemainingIo ioe)

            ErrorE err ->
                (state, Print $ pretty $ SemanticErrorU err)


testArg =
    W
    { startW = RuntimeP
    , valueW =
        List
        [ W
            { startW = RuntimeP
            , valueW = AllV
            , endW = RuntimeP
            }
        , W
            { startW = RuntimeP
            , valueW = AllInputV
            , endW = RuntimeP
            }
        ]
    }


evaluate :: W -> Evaluated
evaluate w =
    case valueW w of
    Map m ->
        evaluateMap w m

    List ls ->
        evaluateList w ls


evaluateMap :: W -> M.Map W (Access W) -> Evaluated
evaluateMap context map_ =
    evaluateMapHelp context (M.toList map_) M.empty


evaluateMapHelp
    :: W
    -> [(W, Access W)]
    -> M.Map W (Access W)
    -> Evaluated
evaluateMapHelp context old new =
    case old of
    [] ->
        EvaluatedE $ context { valueW = Map new }

    (ku, Public vu):ld ->
        case (evaluate ku, evaluate vu) of
        (EvaluatedE ke, EvaluatedE ve) ->
            evaluateMapHelp
                context
                ld
                (M.insert ke (Public ve) new)

        (ErrorE err, _) ->
            ErrorE err

        (_, ErrorE err) ->
            ErrorE err

        (IoE ioe, _) ->
            IoE ioe

        (_, IoE ioe) ->
            IoE ioe

    (ku, Private vu):ld ->
        case (evaluate ku, evaluate vu) of
        (EvaluatedE ke, EvaluatedE ve) ->
            evaluateMapHelp
                context
                ld
                (M.insert ke (Private ve) new)

        (ErrorE err, _) ->
            ErrorE err

        (_, ErrorE err) ->
            ErrorE err

        (IoE ioe, _) ->
            IoE ioe

        (_, IoE ioe) ->
            IoE ioe


evaluateList :: W -> [W] -> Evaluated
evaluateList lsU ls =
    evaluateListHelp lsU ls []


evaluateListHelp :: W -> [W] -> [W] -> Evaluated
evaluateListHelp lsU old new =
    case old of
    [] ->
        EvaluatedE $ lsU { valueW = List $ reverse new }

    o:ld ->
        case evaluate o of
        EvaluatedE oE ->
            evaluateListHelp lsU ld (oE : new)

        ErrorE err ->
            ErrorE err

        IoE ioe ->
            IoE ioe


data Evaluated
    = ErrorE SemanticError
    | EvaluatedE W
    | IoE Output


parse :: T.Text -> Either ParseError W
parse =
    undefined


data ParseError


data ToUser
    = BadDoc UserBad UserDoc
    | BadInternal InternalError
    | Good Go
    | ParseErrorU ParseError
    | SemanticErrorU SemanticError


data InternalError
    = RemainingIo Output
    | NoCodePath


data SemanticError
    = NoSuchName W
    | NotAMap W
    | BadType W V


data UserBad
    = NoArgs
    | TooManyArgs
    | NoCodeFile IOError String
    | BadEncoding UnicodeException String


prettyBad :: UserBad -> T.Text
prettyBad bad =
    ("Error: " <>) $
    case bad of
    NoArgs ->
        "No command-line arguments."

    TooManyArgs ->
        "More than one command-line argument."

    NoCodeFile err path ->
        "Couldn't open file " <>
        T.pack path <>
        ":\n" <>
        T.pack (show err)

    BadEncoding err path ->
        "Couldn't decode file " <>
        T.pack path <>
        ":\n" <>
        T.pack (show err)
        


data UserDoc
    = ArgsDoc
    | EncodingDoc


prettyDoc :: UserDoc -> T.Text
prettyDoc doc =
    case doc of
    ArgsDoc ->
        "Put the name of the code file as a command-line argument."

    EncodingDoc ->
        "Code files must be UTF-8."


data Go
    = Go


pretty :: ToUser -> T.Text
pretty toUser =
    case toUser of
    BadDoc bad doc ->
        prettyBad bad <> "\n\n" <> prettyDoc doc

    Good go ->
        prettyGo go

    BadInternal err ->
        "Sorry, it's just not working:\n" <> prettyBadInternal err

    ParseErrorU err ->
        prettyParseError err

    SemanticErrorU err ->
        prettySemanticError err


prettyBadInternal :: InternalError -> T.Text
prettyBadInternal =
    undefined


prettySemanticError :: SemanticError -> T.Text
prettySemanticError =
    undefined


prettyParseError :: ParseError -> T.Text
prettyParseError =
    undefined


prettyGo :: Go -> T.Text
prettyGo go =
    case go of
    Go ->
        "go!"
