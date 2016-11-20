module Clock exposing (Clock, withPeriod, update)

{-| Clock is designed to work with [elm-lang/animation-frame](package.elm-lang.org/packages/elm-lang/animation-frame/latest/AnimationFrame). Your model will get consistently-paced updates, despite fluctuations in frame diffs.

@docs Clock, withPeriod, update
-}

import Time exposing (Time)


{-| A clock.
-}
type Clock
    = Clock
        { lag : Time
        , period : Time
        }


{-| Create a clock that updates with the given real-time period.

    withDelta (100 * Time.millisecond) -- calls the tick function ten times per second
-}
withPeriod : Time -> Clock
withPeriod period =
    Clock
        { lag = 0
        , period = period
        }


{-| Called like so:

    update up diff clock model

The diff is a real-time diff, such as what is given by AnimationFrame.diffs. This function will pass the diff to the clock. If the diff causes the clock's counter to increment, then `up` will be called with the period and the model.
-}
update : (Time -> a -> a) -> Time -> Clock -> a -> ( Clock, a )
update up dt (Clock clock) model =
    let
        reduceLag ( Clock c, m ) =
            if c.lag < c.period then
                ( Clock c, m )
            else
                reduceLag
                    ( Clock { c | lag = c.lag - c.period }
                    , up c.period m
                    )
    in
        reduceLag
            ( Clock { clock | lag = clock.lag + dt }
            , model
            )
