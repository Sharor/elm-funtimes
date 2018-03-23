module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Set exposing (..)
import String exposing (..)
import Char


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type GameState
    = Lost
    | Won
    | Ongoing


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


buttonEnabled : Char -> Html Msg
buttonEnabled char =
    button
        [ style
            [ ( "backgroundColor", "lightblue" )
            , ( "height", "30px" )
            , ( "width", "30px" )
            , ( "margin", "5px" )
            ]
        , onClick (Guess char)
        ]
        [ text (String.fromChar char) ]


buttonDisabled : Char -> Html Msg
buttonDisabled char =
    button
        [ style
            [ ( "backgroundColor", "grey" )
            , ( "height", "30px" )
            , ( "width", "30px" )
            , ( "margin", "5px" )
            ]
        , disabled True
        ]
        [ text (String.fromChar char) ]


createButton : Set Char -> Char -> Html Msg
createButton tried char =
    if member char tried then
        buttonDisabled char
    else
        buttonEnabled char


alphabet : List Char
alphabet =
    charRange 'A' 'Z'


alphabuttons : Set Char -> Html Msg
alphabuttons tried =
    div [] (List.map (createButton tried) alphabet)


misses : String -> Set Char -> Int
misses word guesses =
    --Set.size (Set.filter (Basics.always True) guesses)
    Set.size (Set.diff guesses (Set.fromList (String.toList word)))


contains : String -> Char -> Bool
contains word c =
    String.contains (toString c) word


gamestate : Model -> GameState
gamestate model =
    if (misses model.word model.tried) > 5 then
        Lost
    else if (showword model) == model.word then
        Won
    else
        Ongoing


view : Model -> Html Msg
view model =
    case (gamestate model) of
        Lost ->
            div [] [ text "YOU ARE DEAD!" ]

        Ongoing ->
            div []
                [ div [] [ text (toString model.tried) ]
                , alphabuttons model.tried
                , div [] [ text (showword model) ]
                , div [] [ text (toString (misses model.word model.tried)) ]
                ]

        Won ->
            div [] [ text "YOU WON!" ]
