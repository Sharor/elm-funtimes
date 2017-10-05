module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)
import Set exposing (..)


suite : Test
suite =
    --    todo "Implement our first test. See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!"
    describe "The Hangman module"
        [ test "maskletter returns known letter" <|
            \_ ->
                let
                    ltr =
                        'C'

                    known =
                        Set.fromList [ 'C', 'D', 'E' ]
                in
                    Expect.equal (maskletter known ltr) ltr
          -- Expect.equal is designed to be used in pipeline style, like this.
        , test "maskletter returns dash for unknown letter" <|
            \_ ->
                let
                    ltr =
                        'Q'

                    known =
                        Set.fromList [ 'C', 'D', 'E' ]
                in
                    Expect.equal (maskletter known ltr) '_'
        , test "convertword masks unknown letters" <|
            \_ ->
                let
                    str =
                        "HELLOWORLD"

                    known =
                        Set.fromList [ 'C', 'D', 'E' ]
                in
                    Expect.equal (convertWord known str) "_E_______D"
        , test "misses base case" <|
            \_ ->
                let
                    str =
                        "HELLOWORLD"

                    known =
                        Set.fromList []
                in
                    Expect.equal (misses str known) 0
        , test "count misses - all wrong" <|
            \_ ->
                let
                    str =
                        "HELLOWORLD"

                    known =
                        Set.fromList [ 'X', 'Y' ]
                in
                    Expect.equal (misses str known) 2
        , test "count misses - all correct" <|
            \_ ->
                let
                    str =
                        "HELLOWORLD"

                    known =
                        Set.fromList [ 'H', 'R' ]
                in
                    Expect.equal (misses str known) 0
        , test "count misses - some wrong" <|
            \_ ->
                let
                    str =
                        "HELLOWORLD"

                    known =
                        Set.fromList [ 'C', 'D', 'E' ]
                in
                    Expect.equal (misses str known) 1
        ]
