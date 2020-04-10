module Eval exposing (eval, parse)


type Item
    = Value Float
    | F1 (Float -> Float)
    | F2 (Float -> Float -> Float)


type Stack
    = Stack (List Item)


emptyStack =
    Stack []


push : Item -> Stack -> Stack
push item (Stack data) =
    Stack (item :: data)


parse : String -> Stack
parse str =
    emptyStack


eval : Stack -> Maybe Float
eval stack =
    Nothing
