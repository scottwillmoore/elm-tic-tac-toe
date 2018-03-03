module State exposing (init, update, subscriptions)

import Dict exposing (Dict)
import Types exposing (..)


defaultModel : Model
defaultModel =
    { turn = O
    , board = Dict.empty
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


attemptPlacement : Model -> Board -> Player -> Position -> Model
attemptPlacement model board player position =
    let
        cell =
            Dict.get position board
    in
        case cell of
            Nothing ->
                { model
                    | turn = swapPlayer player
                    , board = Dict.insert position player board
                }

            Just player ->
                model


checkStraight : Board -> Player -> List Position -> Bool
checkStraight board player straight =
    straight |> List.all (\position -> (Dict.get position board) == Just player)


checkState : Board -> Status
checkState board =
    Play


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( defaultModel, Cmd.none )

        CellClicked position ->
            let
                nextModel =
                    attemptPlacement model model.board model.turn position

                state =
                    checkState nextModel.board
            in
                case state of
                    Win player ->
                        ( defaultModel, Cmd.none )

                    Draw ->
                        ( defaultModel, Cmd.none )

                    Play ->
                        ( nextModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
