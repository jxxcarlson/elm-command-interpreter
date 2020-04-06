port module Command exposing
    ( displayRegister
    , echo
    , f1
    , f2
    , get
    , help
    , message
    , op
    , put
    , rcl
    , sto
    )

import ArgList exposing (ArgList)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Model exposing (Model, Msg(..))


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


precision =
    4


getRegister : Model -> String -> Maybe Float
getRegister model registerName =
    case registerName of
        "a" ->
            model.registerA

        "b" ->
            model.registerB

        "c" ->
            model.registerC

        "d" ->
            model.registerD

        "e" ->
            model.registerE

        "f" ->
            model.registerF

        "m" ->
            model.registerM

        _ ->
            model.registerM


roundTo : Int -> Float -> Float
roundTo d x =
    let
        factor =
            10.0 ^ toFloat d
    in
    x * factor |> round |> (\u -> toFloat u / factor)


getRegisterAsString : Model -> String -> String
getRegisterAsString model registersName =
    case getRegister model registersName of
        Nothing ->
            "undefined"

        Just x ->
            x |> roundTo precision |> String.fromFloat


setRegisterFromString : String -> String -> Model -> Model
setRegisterFromString registerName registerContents model =
    setRegister registerName (String.toFloat registerContents) model


setRegister : String -> Maybe Float -> Model -> Model
setRegister registerName registerContents model =
    case registerName of
        "a" ->
            { model | registerA = registerContents }

        "b" ->
            { model | registerB = registerContents }

        "c" ->
            { model | registerC = registerContents }

        "d" ->
            { model | registerD = registerContents }

        "e" ->
            { model | registerE = registerContents }

        "f" ->
            { model | registerF = registerContents }

        "m" ->
            { model | registerM = registerContents }

        _ ->
            model


displayRegister : Model -> ArgList -> String -> ( Model, Cmd Msg )
displayRegister model argList _ =
    let
        reg_ =
            ArgList.get 0 argList

        reg =
            if reg_ == "_none_" then
                "m"

            else
                reg_
    in
    model |> displayRegisterContents (String.toUpper reg) (getRegisterAsString model reg)


f1 : Model -> (Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f1 model f argList =
    case ArgList.getAsFloat 0 argList of
        Nothing ->
            model |> withCmd (put "argument is not a number")

        Just a ->
            let
                value =
                    f a

                valueAsString =
                    roundTo precision value |> String.fromFloat
            in
            setRegister "m" (Just value) model |> withCmd (put valueAsString)


f2 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
f2 model f argList =
    case ( ArgList.getAsFloat 0 argList, ArgList.getAsFloat 1 argList ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "first argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "second argument is not a number")

        ( Just a, Just b ) ->
            let
                value =
                    f a b

                valueAsString =
                    roundTo precision value |> String.fromFloat
            in
            setRegister "m" (Just value) model |> withCmd (put valueAsString)


op : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
op model op_ argList =
    case ArgList.length argList of
        2 ->
            op2 model op_ argList

        1 ->
            op1 model op_ argList

        _ ->
            model |> withCmd (put "must give one or two arguments")


op1 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
op1 model op_ argList =
    case ( ArgList.getAsFloat 0 argList, getRegister model "m" ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "Register M is empty")

        ( Just a, Just b ) ->
            let
                sum =
                    op_ b a

                sumAsString =
                    roundTo precision sum |> String.fromFloat
            in
            setRegister "m" (Just sum) model |> withCmd (put sumAsString)


op2 : Model -> (Float -> Float -> Float) -> ArgList -> ( Model, Cmd Msg )
op2 model op_ argList =
    case ( ArgList.getAsFloat 0 argList, ArgList.getAsFloat 1 argList ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "first argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "second argument is not a number")

        ( Just a, Just b ) ->
            let
                sum =
                    op_ b a

                sumAsString =
                    roundTo precision sum |> String.fromFloat
            in
            setRegister "m" (Just sum) model |> withCmd (put sumAsString)


echo : Model -> ArgList -> String -> ( Model, Cmd Msg )
echo model _ input =
    model |> withCmd (put ("echo: " ++ input))


message : Model -> String -> ( Model, Cmd Msg )
message model input =
    model |> withCmd (put input)


help : Model -> ( Model, Cmd Msg )
help model =
    model |> withCmd (put helpText)


sto : Model -> ArgList -> String -> ( Model, Cmd Msg )
sto model argList _ =
    let
        reg =
            ArgList.get 0 argList

        message_ =
            "M > " ++ String.toUpper reg
    in
    setRegister reg model.registerM model |> withCmd (put message_)


rcl model argList _ =
    let
        reg =
            ArgList.get 0 argList

        message_ =
            String.toUpper reg ++ " > M"
    in
    case getRegister model reg of
        Nothing ->
            model |> withCmd (put (String.toUpper reg ++ "s empty; no change to M"))

        Just registerContents ->
            setRegister "m" (Just registerContents) model |> withCmd (put message_)


displayRegisterContents : String -> String -> Model -> ( Model, Cmd Msg )
displayRegisterContents registerName registerContents model =
    model |> withCmd (put <| String.toUpper registerName ++ ": " ++ registerContents)


helpText =
    """---------------------------------------------------------------------------------------
Simple command line program: calculator
---------------------------------------------------------------------------------------
Operations: add, mul, etc. as described in the command summary. Saying 'add 2 3'
computes the sum and places the result in register.  If you subsequently say
'add 4', this number will be added to the contents of register M.  The
command `sub 1' will subtract 1 from register M.

This calculator has registers A, B, C, D, E, F, and M, each of which can hold a
floating point number.  Type 'r' to display register M, type 'r a' to
display the contents of register a, etc.

Command summary
---------------------------------------------------------------------------------------
> add a b         -- compute a + b, store result in register M
> mul a b         -- compute a * b, result to register M
> sub a b         -- compute a - b, result to register M
> div a b         -- compute a / b, result to register M
> exp x           -- exponential of x
> ln x            -- natural logarithm of x
> pow a x         -- a^x
> log a x         -- logarithm of x to base a
> d               -- display contents of register M
> d a             -- display contents of register A
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> h               -- show help
---------------------------------------------------------------------------------------
"""
