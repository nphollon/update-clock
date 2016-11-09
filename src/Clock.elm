module Clock exposing (Clock, withPeriod, setPeriod, update)

{-|
@docs Clock, withDelta, setDelta, update
-}

import Time exposing (Time)


{-| A clock.
-}
type Clock
    = Clock
        { lag : Time
        , period : Time
        , time : Int
        }


{-| Create a clock that updates with the given real-time period.

    withDelta 0.1 -- calls the tick function ten times per second
-}
withPeriod : Time -> Clock
withPeriod period =
    Clock
        { lag = 0
        , time = 0
        , period = period
        }


{-| Change a clock's period to the given value.
-}
setPeriod : Time -> Clock -> Clock
setPeriod period (Clock clock) =
    Clock { clock | period = period }


{-| Called like so:

    update up diff clock model

The diff is a real-time diff, such as what is given by AnimationFrame.diffs. This function will pass the diff to the clock. If the diff causes the clock's counter to increment, then `up` will be called with the counter and the model.
-}
update : (Int -> a -> a) -> Time -> Clock -> a -> ( Clock, a )
update up dt (Clock clock) model =
    let
        reduceLag ( Clock c, m ) =
            if c.lag < c.period then
                ( Clock c, m )
            else
                reduceLag
                    ( Clock
                        { c
                            | lag = c.lag - c.period
                            , time = c.time + 1
                        }
                    , up (c.time + 1) m
                    )
    in
        reduceLag
            ( Clock { clock | lag = clock.lag + dt }
            , model
            )