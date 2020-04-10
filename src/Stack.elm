module Stack exposing (Stack(..), empty, init, isEmpty, pop, pop2, push, rot, show, swap, top)

import List.Extra


type Stack a
    = Stack (List a)


init : List a -> Stack a
init data =
    Stack data


empty : Stack a
empty =
    Stack []


isEmpty : Stack a -> Bool
isEmpty (Stack data) =
    data == []


length : Stack a -> Int
length (Stack data) =
    List.length data


top : Stack a -> Maybe a
top (Stack data) =
    List.head data


swap : Stack a -> Stack a
swap (Stack data) =
    case ( List.Extra.getAt 0 data, List.Extra.getAt 1 data ) of
        ( Nothing, _ ) ->
            Stack data

        ( _, Nothing ) ->
            Stack data

        ( Just x, Just y ) ->
            let
                data_ =
                    List.drop 2 data
            in
            Stack (y :: x :: data_)


rot : Stack a -> Stack a
rot (Stack data) =
    case List.Extra.uncons data of
        Nothing ->
            Stack data

        Just ( top_, bottom ) ->
            Stack (bottom ++ [ top_ ])


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
