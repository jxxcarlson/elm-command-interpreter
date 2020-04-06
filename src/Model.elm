module Model exposing (Flags, Model, Msg(..), initModel)


type alias Model =
    { registerA : Maybe Float
    , registerB : Maybe Float
    , registerC : Maybe Float
    , registerD : Maybe Float
    , registerE : Maybe Float
    , registerF : Maybe Float
    , registerM : Maybe Float
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
    }


type Msg
    = Input String
