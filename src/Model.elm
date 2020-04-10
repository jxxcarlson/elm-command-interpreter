module Model exposing (Flags, Model, Msg(..), initModel)

import Stack exposing (Stack)


type alias Model =
    { registerA : Maybe Float
    , registerB : Maybe Float
    , registerC : Maybe Float
    , registerD : Maybe Float
    , registerE : Maybe Float
    , registerF : Maybe Float
    , registerM : Maybe Float
    , stack : Stack Float
    }


type alias Flags =
    ()


initModel : Flags -> Model
initModel _ =
    { registerA = Nothing
    , registerB = Nothing
    , registerC = Nothing
    , registerD = Nothing
    , registerE = Nothing
    , registerF = Nothing
    , registerM = Nothing
    , stack = Stack.empty
    }


type Msg
    = Input String
