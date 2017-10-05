module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import String exposing (..)
import Char


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Msg
    = Guess Char


type alias Model =
    { word : String
    , tried : Set Char
    }


charRange : Char -> Char -> List Char
charRange from to =
    List.map Char.fromCode <| List.range (Char.toCode from) (Char.toCode to)


model : Model
model =
    { word = "HELLOWORLD"
    , tried = Set.empty
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Guess letter ->
            { model
                | tried = Set.insert letter model.tried
            }


showword : Model -> String
showword model =
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


misses : String -> Set Char -> Int
misses word guesses =
    --Set.size (Set.filter (Basics.always True) guesses)
    Set.size (Set.diff guesses (Set.fromList (String.toList word)))


contains : String -> Char -> Bool
contains word c =
    String.contains (toString c) word


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model.tried) ]
        , alphabuttons
        , div [] [ text (showword model) ]
        ]
