module Compiler exposing
    ( init
    , view
    , update
    , subscriptions
    )


import Html
import Browser
import Set
import Dict
import Bytes


type Msg
    = NewCode String


type alias Model =
    { code : String
    , output : Result String String
    }


emptyMessage =
    "write some code in the box"


initModel =
    { code = ""
    , output = Err emptyMessage
    }


init : () -> (Model, Cmd Msg)
init _ =
    (initModel, Cmd.none)


view : Model -> Browser.Document Msg
view _ =
    { title = "tl"
    , body = [Html.text "tl"]
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewCode newCode ->
            onNewCode newCode model


type Value
    = FunctionV (Context Value, Value) (Context Value)
    | FunctionCallV (Context Value) (Context Value)
    | CaseOfV (Context Value) (List (Context Value, Context Value))
    | StringV String
    | LetInV Scope (Context Value)
    | AllV
    | ListV (List (Context Value))
    | AllIoV
    | LookupV (Context Value)
    | AllFunctions


type alias Context a =
    { located : Located a
    , scope : Scope
    }


parse : String -> Result String (Located Value)
parse raw =
    Debug.todo "parse"


onNewCode : String -> Model -> (Model, Cmd Msg)
onNewCode code model =
    case parse code of
        Err err ->
            ( { model
                | output = Err err
                , code = code
              }
            , Cmd.none
            )

        Ok parsed ->
            case fillInScopes (Scope Dict.empty) (getValueL parsed) of
                Err err ->
                    ({ model | output = Err err }, Cmd.none)

                Ok scoped ->
                    case simplify scoped of
                        Err err ->
                            ({model | output = Err err}, Cmd.none)

                        Ok simplified ->
                            if validMainOutput simplified then
                                case refineArgTypes scoped of
                                    Err err ->
                                        ({model | output = Err err}, Cmd.none)

                                    Ok refined ->
                                        ( { model | output = Ok (makeGo refined) }
                                        , Cmd.none
                                        )

                            else
                                badMainOutput simplified


simplify : Context Value -> Result String (Context Value)
simplify =
    Debug.todo "simplify"


makeGo =
    Debug.todo "makeGo"


getValueL : Located a -> a
getValueL l =
    case l of
        InCode i ->
            i.value

        InRuntime i ->
            i


badValue : Value -> Located Value -> String
badValue expected got =
    String.concat
        [ "wrong value: expected "
        , showValue expected
        , " but got "
        , showValue <| getValueL got
        , prettyLocation got
        ]


prettyLocation : Located a -> String
prettyLocation located =
    case located of
        InCode inCode ->
            String.concat
                [ "between "
                , prettyPos inCode.start
                , " and "
                , prettyPos inCode.end
                ]

        InRuntime _ ->
            "runtime"


prettyPos : (Int, Int) -> String
prettyPos (row, col) =
    String.concat
        [ "("
        , String.fromInt row
        , ", "
        , String.fromInt col
        , ")"
        ]


insert : Context Value -> Context Value -> Scope -> Scope
insert key value (Scope oldScope) =
    case key.located of
        InCode location ->
            Scope <|
            Dict.insert
                (encodeValue (getValueC key))
                { nameStart = location.start
                , nameEnd = location.end
                , value = value
                }
                oldScope

        InRuntime _ ->
            Scope oldScope


encodeValue : Value -> String
encodeValue value =
    case value of
        FunctionV param body ->
            String.concat
                [ "func"
                , encodeValue <| getValueC <| Tuple.first param
                , encodeValue <| getValueC body
                ]


getValueC : Context Value -> Value
getValueC c =
    case c.located of
        InRuntime v ->
            v

        InCode i ->
            i.value


generateGo : Value -> String
generateGo value =
    case value of
        FunctionV _ _ ->
            ""


startArg : Context Value
startArg =
    { located =
        InRuntime <|
        ListV
            [ { located = InRuntime AllV
              , scope = Scope Dict.empty
              }
            , { located = InRuntime AllIoV
              , scope = Scope Dict.empty
              }
            ]
    , scope = Scope Dict.empty
    }


refineArgTypes : Context Value -> Result String (Context Value)
refineArgTypes =
    Debug.todo "refineArgTypes"
            

validMainOutput out =
            case getValueC out of
                ListV [_, ioC] ->
                    isSubType (getValueC ioC) AllIoV

                _ ->
                    False


badMainOutput bad =
    String.concat
        [ "the output of the main function should be "
        , showValue expectedMainOut
        , " but is "
        , showValue bad
        ]


expectedMainOut =
    ListV
        [ {located = InRuntime AllV, scope = Scope Dict.empty}
        , {located = InRuntime AllIoV, scope = Scope Dict.empty}
        ]


mainOutputNotIo : Value -> String
mainOutputNotIo notIo =
    String.concat
        [ "second element of output of main function should be "
        , showValue AllIoV
        , " but is "
        , showValue notIo
        ]


isSubType : Value -> Value -> Bool
isSubType sub super =
    case (sub, super) of
        (FunctionV _ _, FunctionV _ _) ->
            sub == super

        (FunctionV _ _, AllV) ->
            True


showValue : Value -> String
showValue value =
    case value of
        FunctionV param body ->
            "function"


fillInScopes : Context Value -> Result String (Context Value) 
fillInScopes value =
    case getValueC value of
        FunctionV param body ->
            Ok
                { located = 
            FunctionV param body

        FunctionCallV candidate arg ->
            Ok <| FunctionCallV candidate arg
            

badEnd : Value -> String
badEnd got =
    String.concat
        [ "first element of output of main function should be an "
        , "io action, but it was: "
        , showValue got
        ]


type Scope
    = Scope (Dict.Dict String Scoped)


type alias Scoped = 
    { nameStart : (Int, Int)
    , nameEnd : (Int, Int)
    , value : Context Value
    }


showPosition : (Int, Int) -> String
showPosition (row, col) =
    String.concat
    [ "("
    , String.fromInt row
    , ", "
    , String.fromInt col
    , ")"
    ]


type Located a
    = InCode
        { start : (Int, Int)
        , value : a
        , end : (Int, Int)
        }
    | InRuntime a


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
