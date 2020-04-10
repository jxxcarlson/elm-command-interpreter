port module Command exposing
    ( average
    , clear
    , f1
    , f2
    , get
    , handleNumber
    , help
    , list
    , memclear
    , pop
    , put
    , rcl
    , rot
    , showMem
    , showMessage
    , showStack
    , showStackTop
    , sto
    , sum
    , swap
    )

import ArgList exposing (ArgList)
import Cmd.Extra exposing (withCmd, withNoCmd)
import FloatStack exposing (..)
import Model exposing (Model, Msg(..))
import Stack exposing (BoundedInt(..), Stack)



-- PORTS


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg



-- FLOATING POINT STUFF


precision =
    4


roundTo : Int -> Float -> Float
roundTo d x =
    let
        factor =
            10.0 ^ toFloat d
    in
    x * factor |> round |> (\u -> toFloat u / factor)



-- FUNCTIONS TO MANAGE THE STACK


list : Model -> ArgList -> ( Model, Cmd Msg )
list model argList =
    showMessage { model | stack = Stack.pushList (ArgList.getFloatList argList) model.stack } "list pushed onto stack"


clear : Model -> ( Model, Cmd Msg )
clear model =
    showMessage { model | stack = Stack.empty } "Stack empty"


sto : Model -> ( Model, Cmd Msg )
sto model =
    showMessage { model | memory = model.stack } "stack > memory"


rcl : Model -> ( Model, Cmd Msg )
rcl model =
    showMessage { model | stack = Stack.pushStack model.memory model.stack } "stack = memory ++ stack"


memclear : Model -> ( Model, Cmd Msg )
memclear model =
    showMessage { model | memory = Stack.empty } "memory clear"


pop : Model -> ( Model, Cmd Msg )
pop model =
    let
        ( maybeFloat, newStack ) =
            Stack.pop model.stack

        message =
            case maybeFloat of
                Nothing ->
                    "stack is empty"

                Just x ->
                    String.fromFloat << roundTo precision <| x
    in
    showMessage { model | stack = newStack } message


handleNumber : Model -> String -> ( Model, Cmd Msg )
handleNumber model str =
    case String.toFloat str of
        Nothing ->
            model |> withCmd (put "not a number")

        Just x ->
            let
                newStack =
                    Stack.push x model.stack
            in
            { model | stack = newStack } |> withCmd (put (Stack.show (String.fromFloat << roundTo precision) newStack))


showStack : Model -> ( Model, Cmd Msg )
showStack model =
    case Stack.isEmpty model.stack of
        True ->
            showMessage model "stack is empty"

        False ->
            model |> withCmd (put <| "stack: " ++ Stack.show (String.fromFloat << roundTo precision) model.stack)


showMem : Model -> ( Model, Cmd Msg )
showMem model =
    case Stack.isEmpty model.stack of
        True ->
            showMessage model "memory is empty"

        False ->
            model |> withCmd (put <| "memory: " ++ Stack.show (String.fromFloat << roundTo precision) model.memory)


showStackTop : Model -> ( Model, Cmd Msg )
showStackTop model =
    case Stack.top model.stack of
        Nothing ->
            showMessage model "stack is empty"

        Just x ->
            showMessage model <| (String.fromFloat << roundTo precision) <| x


showMessage : Model -> String -> ( Model, Cmd Msg )
showMessage model msg =
    model |> withCmd (put msg)



-- EVALUATE FUNCTIONS AND OPERATIONS


f1 : Model -> (Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f1 model f argList =
    case ArgList.length argList of
        1 ->
            case ArgList.getAsFloat 0 argList of
                Nothing ->
                    showMessage model "argument is not a number"

                Just x ->
                    f11 model f x

        0 ->
            f10 model f

        _ ->
            showMessage model "function requires at most one argument"


f10 : Model -> (Float -> Float) -> ( Model, Cmd Msg )
f10 model f =
    case Stack.pop model.stack of
        ( Nothing, _ ) ->
            showMessage model "stack is empty"

        ( Just x, stack ) ->
            let
                value =
                    f x

                newStack =
                    Stack.push value stack
            in
            { model | stack = newStack } |> showStackTop


f11 : Model -> (Float -> Float) -> Float -> ( Model, Cmd Msg )
f11 model f x =
    let
        value =
            f x

        newStack =
            Stack.push value model.stack
    in
    { model | stack = newStack } |> showStackTop


f2 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f2 model op_ argList =
    case ArgList.length argList of
        2 ->
            f22 model op_ argList

        1 ->
            f21 model op_ argList

        0 ->
            f20 model op_

        _ ->
            model |> withCmd (put "must give no more than two arguments")


f20 : Model -> (Float -> Float -> Float) -> ( Model, Cmd Msg )
f20 model op_ =
    let
        ( x, y, st ) =
            Stack.pop2 model.stack
    in
    case ( x, y, st ) of
        ( Nothing, _, _ ) ->
            model |> withCmd (put "Not enough data in the stack (1)")

        ( _, Nothing, _ ) ->
            model |> withCmd (put "Not enough data in the stack (2)")

        ( Just x_, Just y_, st_ ) ->
            let
                z =
                    op_ x_ y_

                newStack =
                    Stack.push z st_
            in
            { model | stack = newStack } |> showStackTop


f21 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f21 model op_ argList =
    case ArgList.getAsFloat 0 argList of
        Nothing ->
            model |> withCmd (put "argument is not a number")

        Just x ->
            case Stack.pop model.stack of
                ( Nothing, _ ) ->
                    showMessage model "stack is too short"

                ( Just y, stack ) ->
                    let
                        z =
                            op_ x y

                        newStack =
                            Stack.push z stack
                    in
                    { model | stack = newStack } |> showStackTop


f22 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f22 model op_ argList =
    case ( ArgList.getAsFloat 0 argList, ArgList.getAsFloat 1 argList ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "first argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "second argument is not a number")

        ( Just x, Just y ) ->
            let
                z =
                    op_ x y

                newStack =
                    Stack.push z model.stack
            in
            { model | stack = newStack } |> showStackTop


average : Model -> ( Model, Cmd Msg )
average model =
    let
        ( _, st ) =
            FloatStack.average model.stack
    in
    { model | stack = st } |> showStackTop


sum : Model -> ( Model, Cmd Msg )
sum model =
    let
        ( _, st ) =
            FloatStack.sum model.stack
    in
    { model | stack = st } |> showStackTop


swap : Model -> ( Model, Cmd Msg )
swap model =
    { model | stack = Stack.swap model.stack } |> showStack


rot : Model -> ( Model, Cmd Msg )
rot model =
    { model | stack = Stack.rot model.stack } |> showStack


help : Model -> ( Model, Cmd Msg )
help model =
    model |> withCmd (put helpText)


helpText =
    """---------------------------------------------------------------------------------------
Simple command line program: stack-based calculator (Work in progress)
---------------------------------------------------------------------------------------
Arithmetic
----------
Binary operations are add, mul, sub, and div
Unary operations are neg and recip

> 1               -- put 2 on top of the stack
> add 2           -- add 2 to the top of stack
> add 3 5         -- add 3 and 5, put result on top of the stack
> add             -- add the top two numbers on the stack
---------------------------------------------------------------------------------------
Stack
-----
> s               -- show the stack
> pop             -- pop an element off the stack
> swap            -- swap the top two elements of the stack
> rot             -- rotate stack
> sum             -- sum of values on stack
> av              -- average of values on stack
> sto             -- copy the stack to memory
> rcl             -- push the contents of the memory onto the stack
> c               -- clear stack
> mc              -- clear memory
> m               -- show memory
> list 1 2 3      -- push 1, 2, 3 onto stack
---------------------------------------------------------------------------------------
Functions
---------
> exp 2           -- exponential of 2, put the result on the stack
> ln 2            -- natural logarithm of 2, put the result on the stack
> log 2 16        -- base 2 logarithm of 16, put the result on the stack
> pow 2 16        -- 2^16, put the result on the stack
---------------------------------------------------------------------------------------
"""
