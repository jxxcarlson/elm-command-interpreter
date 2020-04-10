module FloatStack exposing (average, sum)

import Stack exposing (BoundedInt(..), Stack)


sum : Stack Float -> ( Float, Stack Float )
sum stack =
    let
        ( xs, _ ) =
            Stack.take All stack

        sum_ =
            List.sum xs
    in
    ( sum_, Stack.init [ sum_ ] )


average : Stack Float -> ( Float, Stack Float )
average stack =
    let
        ( s, _ ) =
            sum stack

        n =
            Stack.depth stack |> toFloat

        av =
            s / n
    in
    ( av, Stack.init [ av ] )
