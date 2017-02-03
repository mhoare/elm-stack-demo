module Types exposing (..)

import Array exposing (..)


type alias StackData a =
    Array a


type Msg
    = Pop
    | Temp String
    | Push
