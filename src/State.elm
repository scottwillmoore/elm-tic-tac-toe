module State exposing (init, update, subscriptions)

import Dict exposing (Dict)
import Types exposing (..)
import Board


defaultModel : Model
defaultModel =
    { turn = O
    , board = Dict.empty
    , status = Play
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


swapPlayer : Player -> Player
swapPlayer player =
    case player of
        X ->
            O

        O ->
            X


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( defaultModel, Cmd.none )

        CellClicked position ->
            let
                ( nextBoard, placed ) =
                    Board.place model.board model.turn position

                nextTurn =
                    if placed then
                        swapPlayer model.turn
                    else
                        model.turn

                nextStatus =
                    Board.status nextBoard

                nextModel =
                    { model | board = nextBoard, turn = nextTurn, status = nextStatus }
            in
                if nextStatus == Play then
                    ( nextModel, Cmd.none )
                else
                    ( defaultModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
