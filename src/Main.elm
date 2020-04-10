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
        "clear" ->
            Command.clear model

        "pop" ->
            Command.pop model

        "add" ->
            Command.f2 model (+) args

        "mul" ->
            Command.f2 model (*) args

        "sub" ->
            Command.f2 model (-) args

        "div" ->
            Command.f2 model (/) args

        "exp" ->
            Command.f1 model (\x -> e ^ x) args

        "ln" ->
            Command.f1 model (logBase e) args

        "neg" ->
            Command.f1 model (\x -> -x) args

        "pow" ->
            Command.f2 model (\a b -> a ^ b) args

        "log" ->
            Command.f2 model logBase args

        "recip" ->
            Command.f1 model (\x -> 1 / x) args

        "rot" ->
            Command.rot model

        "swap" ->
            Command.swap model

        "h" ->
            Command.help model

        "s" ->
            Command.showStack model

        _ ->
            case List.member (String.left 1 cmd) numerals of
                True ->
                    Command.handleNumber model cmd

                False ->
                    case cmd == "" of
                        True ->
                            Command.showMessage model "stack is empty"

                        False ->
                            Command.showMessage model "I don't understand"


numerals =
    String.split "" "0123456789"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Command.get Input
