port module Main exposing (main)

import Html
import Element as E
import Browser
import Json.Encode as Je
import Json.Decode as Jd


main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type Msg
    = FromJsM Je.Value


port fromJs : (Je.Value -> Msg) -> Sub Msg


port toJs : Je.Value -> Cmd Msg


subscriptions : Model -> Cmd Msg
subscriptions _ =
    fromJs FromJsM


init : () -> (Model, Cmd Msg)
init _ =
    (initModel, Cmd.none)


view : Model -> Html.Html Msg
view model =
    E.layout [] (viewE model)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FromJsM raw ->
            case Jd.decodeValue fromJsDecoder raw of
                Err err ->
                    internalError (Jd.errorToString err)

                Ok ok ->
                    (ok, Cmd.none)


fromJsDecoder : Jd.Decoder FromJs
fromJsDecoder =
    Jd.map2 FromJs
        (Jd.field "box" Jd.string)
        (Jd.field "below" <| Jd.list belowD)


belowD : Jd.Decoder Below
belowD =
    Jd.map2 Below
        (Jd.field "type" belowTypeD)
        (Jd.field "value" Jd.string)


belowTypeD =
    Jd.oneOf
    [ Jd.andThen (Jd.succeed TextT) (Jd.field "text")
    ]


internalError : String -> (Model, Cmd Msg)
internalError err =
    ( {box = "", below = [TextT <| "Internal error: " ++ err]}
    , Cmd.none
    )


type alias Model =
    FromJs
    

initModel =
    { box = ""
    , below = []
    }


type alias FromJs =
    { box : String
    , below : List Below
    }


type alias Below =
    { type_ : BelowType
    , value : String
    }


type BelowType
    = TextT
    | ImageT


viewE : Model -> E.Element Msg
viewE model =
    E.column
    []
    ( Ei.search
        { onChange = BoxChangeM
        , text = model.box
        , placeholder = Nothing
        , label = Ei.labelHidden ""
        }
    ) :: (List.map showToUser model.below)
    

showToUser : Below -> E.Element Msg
showToUser below =
    case below.type_ of
    TextT ->
        E.text below.value

    ImageT ->
        E.image
        { src = below.value        
        , 
