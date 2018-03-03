module App exposing (main)

import Html exposing (program)
import Dict exposing (Dict)
import Types exposing (..)
import Board exposing (..)
import State exposing (..)
import View exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
