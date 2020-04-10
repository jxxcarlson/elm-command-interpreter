module Model exposing (Flags, Model, Msg(..), initModel)

import Stack exposing (Stack)


type alias Model =
    { stack : Stack Float
    , memory : Stack Float
    }


type alias Flags =
    ()


initModel : Flags -> Model
initModel _ =
    { stack = Stack.empty
    , memory = Stack.empty
    }


type Msg
    = Input String
