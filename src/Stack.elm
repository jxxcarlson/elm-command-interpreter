module Stack exposing (Stack(..), empty, init, pop, pop2, push, show)


type Stack a
    = Stack (List a)


init : List a -> Stack a
init data =
    Stack data


empty : Stack a
empty =
    Stack []


pop : Stack a -> ( Maybe a, Stack a )
pop (Stack data) =
    ( List.head data, Stack (List.drop 1 data) )


pop2 : Stack a -> ( Maybe a, Maybe a, Stack a )
pop2 (Stack data) =
    let
        first =
            List.head data

        second =
            List.head (List.drop 1 data)

        newData =
            List.drop 2 data
    in
    ( first, second, Stack newData )


push : a -> Stack a -> Stack a
push x (Stack data) =
    Stack (x :: data)


show : (a -> String) -> Stack a -> String
show displayElement (Stack data) =
    List.map displayElement data |> String.join ", "
