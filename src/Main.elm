module Main exposing (main)

{-| A simple Platform.worker program which
provides a command-line intepreter:
-}

import ArgList exposing (ArgList)
import Cmd.Extra exposing (withNoCmd)
import Command
import Model exposing (Flags, Model, Msg(..), initModel)
import Platform exposing (Program)


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \f -> ( initModel f, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            commandProcessor model input


commandProcessor : Model -> String -> ( Model, Cmd Msg )
commandProcessor model input =
    let
        args_ =
            input |> String.words |> List.map String.trim

        args =
            List.drop 1 args_
                |> ArgList.init
    in
    case List.head args_ of
        Nothing ->
            model |> withNoCmd

        Just cmd ->
            let
                input_ =
                    String.dropLeft (String.length cmd) input
                        |> String.trim
            in
            executeCommand model cmd args input_


executeCommand : Model -> String -> ArgList -> String -> ( Model, Cmd Msg )
executeCommand model cmd args input =
    case cmd of
        "add" ->
            Command.op model (+) args

        "mul" ->
            Command.op model (*) args

        "sub" ->
            Command.op model (-) args

        "div" ->
            Command.op model (/) args

        "exp" ->
            Command.f1 model (\x -> e ^ x) args

        "ln" ->
            Command.f1 model (logBase e) args

        "pow" ->
            Command.f2 model (\a b -> a ^ b) args

        "log" ->
            Command.f2 model logBase args

        "echo" ->
            Command.echo model args input

        "r" ->
            Command.displayRegister model args input

        "h" ->
            Command.help model

        "rcl" ->
            Command.rcl model args input

        "sto" ->
            Command.sto model args input

        _ ->
            Command.message model "I don't understand"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Command.get Input