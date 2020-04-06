port module Command exposing
    ( add
    , displayRegister
    , echo
    , get
    , help
    , message
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
    3


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


add : Model -> ArgList -> ( Model, Cmd Msg )
add model argList =
    case ArgList.length argList of
        2 ->
            add2 model argList

        1 ->
            add1 model argList

        _ ->
            model |> withCmd (put "must give one or two arguments")


add1 : Model -> ArgList -> ( Model, Cmd Msg )
add1 model argList =
    case ( ArgList.getAsFloat 0 argList, getRegister model "m" ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "Register M is empty")

        ( Just a, Just b ) ->
            let
                sum =
                    a + b

                sumAsString =
                    roundTo precision sum |> String.fromFloat
            in
            setRegister "m" (Just sum) model |> withCmd (put sumAsString)


add2 : Model -> ArgList -> ( Model, Cmd Msg )
add2 model argList =
    case ( ArgList.getAsFloat 0 argList, ArgList.getAsFloat 1 argList ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "first argument is not a number")

        ( _, Nothing ) ->
            model |> withCmd (put "second argument is not a number")

        ( Just a, Just b ) ->
            let
                sum =
                    a + b

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
This calculator has registers A, B, C, D, E, F, and M, each of which can hold a
floating point number.  Type 'r' to display register M, type 'r a' to
display the contents of register a, etc.

Command summary
---------------------------------------------------------------------------------------
> d               -- display contents of register M
> d a             -- display contents of register As
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> h               -- show help
---------------------------------------------------------------------------------------
"""
