module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import String exposing (..)
import Char


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Msg
    = Increment
    | Decrement
    | Guess Char


type alias Model =
    { word : String
    , tried : Set Char
    , misses : Int
    }


charRange : Char -> Char -> List Char
charRange from to =
    List.map Char.fromCode <| List.range (Char.toCode from) (Char.toCode to)


model : Model
model =
    { word = "HELLOWORLD"
    , tried = Set.empty
    , misses = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model
                | misses = model.misses + 1
            }

        Decrement ->
            { model
                | misses = model.misses - 1
            }

        Guess letter ->
            { model
                | tried = Set.insert letter model.tried
            }


showword : Model -> String
showword model =
    --  String.map (maskletter model.tried) model.word
    convertWord model.tried model.word


convertWord : Set Char -> String -> String
convertWord known word =
    String.map (maskletter known) word


maskletter : Set Char -> Char -> Char
maskletter tried letter =
    if member letter tried then
        letter
    else
        '_'


createButton char =
    button [ onClick (Guess char) ] [ text (String.fromChar char) ]


alphabet =
    charRange 'A' 'Z'


alphabuttons =
    div [] (List.map createButton alphabet)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "A" ]
        , div [] [ text (toString model.misses) ]
        , button [ onClick Increment ] [ text "B" ]
        , div [] [ text (toString model.tried) ]
        , alphabuttons
        , div [] [ text (showword model) ]
        ]
