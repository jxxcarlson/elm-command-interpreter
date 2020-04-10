module ArgList exposing (ArgList, get, getAsFloat, getFloatList, init, length)

import List.Extra
import Maybe.Extra



-- CONVENIENCE FUNCTIONS FOR EXRACTING ARGUMENTS


type ArgList
    = ArgList
        { length : Int
        , args : List String
        }


init : List String -> ArgList
init list =
    ArgList { length = List.length list, args = list }


get : Int -> ArgList -> String
get k (ArgList data) =
    List.Extra.getAt k data.args |> Maybe.withDefault "_none_"


getAsFloat : Int -> ArgList -> Maybe Float
getAsFloat k argList =
    get k argList |> String.toFloat


getFloatList : ArgList -> List Float
getFloatList (ArgList data) =
    data.args
        |> List.map String.toFloat
        |> Maybe.Extra.values


length : ArgList -> Int
length (ArgList data) =
    data.length
