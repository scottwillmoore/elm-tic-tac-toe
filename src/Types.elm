module Types exposing (Msg(..), Player(..), Position, Cell, Board, Status(..), Model)

import Dict exposing (Dict)


type Msg
    = Reset
    | CellClicked Position


type Status
    = Play
    | Draw
    | Win Player


type Player
    = X
    | O


type alias Position =
    ( Int, Int )


type alias Cell =
    Maybe Player


type alias Board =
    Dict Position Player


type alias Model =
    { turn : Player
    , board : Board
    , status : Status
    , history : List Cell
    }
