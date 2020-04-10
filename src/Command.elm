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



-- FUNCTIONS TO WORK WITH REGISTERS


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


displayRegister : Model -> String -> ( Model, Cmd Msg )
displayRegister model reg =
    model |> displayRegisterContents (String.toUpper reg) (getRegisterAsString model reg)


displayRegisterContents : String -> String -> Model -> ( Model, Cmd Msg )
displayRegisterContents registerName registerContents model =
    model |> withCmd (put <| String.toUpper registerName ++ ": " ++ registerContents)



-- EVALUATE FUNCTIONS AND OPERATIONS


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



-- REGISTER OPERATIONS


sto : Model -> ArgList -> String -> ( Model, Cmd Msg )
sto model argList _ =
    case ArgList.length argList of
        1 ->
            sto1 model (ArgList.get 0 argList)

        2 ->
            sto2 model (ArgList.get 0 argList) (ArgList.get 1 argList)

        _ ->
            model |> withCmd (put "sto requires 1 or 2 arguments")


sto2 : Model -> String -> String -> ( Model, Cmd Msg )
sto2 model val reg =
    case String.toFloat val of
        Nothing ->
            model |> withCmd (put "argument is not a number")

        Just x ->
            setRegister reg (Just x) model |> withCmd (put <| val ++ " > " ++ reg)


sto1 : Model -> String -> ( Model, Cmd Msg )
sto1 model reg =
    setRegister reg model.registerM model |> withCmd (put <| "M > " ++ reg)


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



-- HELPERS


message : Model -> String -> ( Model, Cmd Msg )
message model input =
    model |> withCmd (put input)



-- OTHER COMMANDS


echo : Model -> ArgList -> String -> ( Model, Cmd Msg )
echo model _ input =
    model |> withCmd (put ("echo: " ++ input))


help : Model -> ( Model, Cmd Msg )
help model =
    model |> withCmd (put helpText)


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
Arithmetic
----------
> add 2 3.1       -- compute 2 + 3.1, store result in register M
> mul 2 3.1       -- compute 2 * 3.1, result to register M
> sub 2 3.1       -- compute 2 - 3.1, result to register M
> div 2 3.1       -- compute 2 / 3.1, result to register M
> neg 3.1         -- -3.1
> recip 3.1       -- 1/3.1
---------------------------------------------------------------------------------------
Functions
---------
> exp 2           -- exponential of 2
> ln 2            -- natural logarithm of 2
> log 2 16        -- base 2 logarithm of 16
> pow 2 16        -- 2^16
---------------------------------------------------------------------------------------
Registers
---------
> a               -- display contents of register A
                  -- likewise for registers B, C, D, E, F, M
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> sto 2.1 e       -- store 2.1 in register E
> h               -- show help
---------------------------------------------------------------------------------------
"""
