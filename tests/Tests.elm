module Tests exposing (..)

import Test exposing (..)
import Expect
import Clock exposing (Clock)


all : Test
all =
    describe "Game Clock"
        [ test "No model update if diff is smaller than period" <|
            \() ->
                Expect.equal
                    "Howdy"
                    (Clock.update up 0.1 clock "Howdy" |> Tuple.second)
        , test "One model update if diffs add up to period" <|
            \() ->
                Expect.equal
                    "Howdy1"
                    (Clock.update up 0.1 clock "Howdy"
                        |> uncurry (Clock.update up 0.9)
                        |> Tuple.second
                    )
        , test "Two model updates if diffs add up to two periods" <|
            \() ->
                Expect.equal
                    "Howdy11"
                    (Clock.update up 0.4 clock "Howdy"
                        |> uncurry (Clock.update up 0.4)
                        |> uncurry (Clock.update up 0.4)
                        |> uncurry (Clock.update up 0.4)
                        |> uncurry (Clock.update up 0.4)
                        |> Tuple.second
                    )
        , test "Two updates if diff is big as two periods" <|
            \() ->
                Expect.equal
                    "Howdy11"
                    (Clock.update up 2.1 clock "Howdy"
                        |> Tuple.second
                    )
        ]


up : Float -> String -> String
up dt model =
    model ++ toString (round dt)


clock : Clock
clock =
    Clock.withPeriod 1.0


usedClock : Clock
usedClock =
    Clock.update up 2.1 clock "oldmodel" |> Tuple.first
