module Stack exposing
    ( BoundedInt(..)
    , Stack
    , depth
    , dup
    , empty
    , init
    , isEmpty
    , pop
    , pop2
    , push
    , pushList
    , pushStack
    , rot
    , show
    , swap
    , take
    , top
    )

import List.Extra


type Stack a
    = Stack (List a)


type BoundedInt
    = All
    | Bounded Int


init : List a -> Stack a
init data =
    Stack data


empty : Stack a
empty =
    Stack []


isEmpty : Stack a -> Bool
isEmpty (Stack data) =
    data == []


depth : Stack a -> Int
depth (Stack data) =
    List.length data


dup : Stack a -> Stack a
dup (Stack data) =
    Stack (data ++ data)


top : Stack a -> Maybe a
top (Stack data) =
    List.head data


{-|

    > Stack.take (Bounded 2) s
    ([1,2],Stack [3,4])

    > Stack.take All s
    ([1,2,3,4],Stack [])

-}
take : BoundedInt -> Stack a -> ( List a, Stack a )
take n (Stack data) =
    case n of
        All ->
            ( data, Stack [] )

        Bounded k ->
            ( List.take k data, Stack (List.drop k data) )


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


pushStack : Stack a -> Stack a -> Stack a
pushStack (Stack data1) (Stack data2) =
    Stack (data1 ++ data2)


pushList : List a -> Stack a -> Stack a
pushList list (Stack data) =
    Stack (list ++ data)


show : (a -> String) -> Stack a -> String
show displayElement (Stack data) =
    List.map displayElement data |> String.join ", "
