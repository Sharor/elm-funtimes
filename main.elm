import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (..)
import String exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type Msg = Increment | Decrement | Guess Char


type alias Model = 
  { word: String
  , tried: Set Char
  , misses: Int}

model : Model
model =
    { word = "helloworld"
    , tried = Set.empty
    , misses = 0
    }

update: Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model 
      | misses = model.misses + 1 }
    Decrement ->
      { model 
      | misses = model.misses - 1 }
    Guess letter -> 
      { model
      | tried = Set.insert letter model.tried  }


showword: Model -> String
showword model = 
--  String.map (maskletter model.tried) model.word
  String.map (maskletter model.tried) model.word

convertWord: String -> String
convertWord word =
  String.map (maskletter (Set.fromList ['C','D','E'])) "helloworld"


maskletter: Set Char -> Char -> Char
maskletter tried letter =
  if member letter tried then
    letter
  else
    '_'

view: Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "A" ]
    , div [] [ text (toString model.misses) ]
    , button [ onClick Increment ] [ text "B" ]
     , div [] [ text (toString model.tried) ]
     , button [ onClick (Guess 'A') ] [ text "A" ] 
     , button [ onClick (Guess 'B') ] [ text "B" ] 
     , button [ onClick (Guess 'C') ] [ text "C" ] 
     , button [ onClick (Guess 'D') ] [ text "D" ] 
     , button [ onClick (Guess 'E') ] [ text "E" ] 
     , div [] [ text (showword model) ]
     , div [] [ text (toString (maskletter (Set.fromList ['E']) 'A'))]
     , div [] [ text (toString (maskletter (Set.fromList ['C','D','E']) 'E'))]
     , div [] [ text (convertWord "ABCDE") ]


    ]
