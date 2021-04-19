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
    , importResolver : Set.Set String
    , files : Dict.Dict String Bytes.Bytes
    , notInScope : Dict.Dict String Value
    , inScope : Dict.Dict String Value
    }


emptyMessage =
    "write some code in the box"


initModel =
    { code = ""
    , output = Err emptyMessage
    , importResolver = Set.empty
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
    = Function (Located String) (Located Value)
    | FunctionCall (Located String) (Located Value)
    | CaseOf (Located Value) (List ((Located Value), (Located Value)))
    | StringV String
    | LetIn
        (Dict.Dict String (Located (), Located Value))
        (Located Value)
    | Module
        (Set.Set (Located String))
        (Set.Set (Located String))
        (Dict.Dict String (Located (), Located Value))
    | ReadFile String
    | All


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

        Ok (Function params body) ->
            evaluateFunctionCall
                { params = params
                , body = body
                , args = [StringV "Start", All]
                }
                { model | code = code }


type alias FunctionCall =
    { param : Located String
    , body : Located Value
    , args : Located Value
    }
    

evaluateFunctionCall : FunctionCall -> Model -> (Model, Cmd Msg)
evaluateFunctionCall f model =
    if List.length f.params /= List.length f.args then
        ( { model
            | output = Err <| String.concat
                [ "wrong number of arguments to function: expecting "
                , String.fromInt <| List.length f.params
                , " but got "
                , String.fromInt <| List.length f.args
                ]
          }
        , Cmd.none
        )

    else
        


type alias Located a =
    { start : Location
    , value : a
    , end : Location
    }


type Location
    = RunTime
    | Source Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
