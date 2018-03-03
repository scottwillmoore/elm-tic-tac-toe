module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Board exposing (..)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type State
    = Winner Player
    | Draw
    | Play


type alias Model =
    { turn : Player
    , board : Board
    }


defaultModel : Model
defaultModel =
    { turn = O
    , board = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = Reset
    | CellClicked Position


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


checkState : Board -> State
checkState board =
    if checkStraight board O majorDiagonal then
        Winner O
    else if checkStraight board X majorDiagonal then
        Winner X
    else
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
                    Winner player ->
                        ( defaultModel, Cmd.none )

                    Draw ->
                        ( defaultModel, Cmd.none )

                    Play ->
                        ( nextModel, Cmd.none )


viewCell : Position -> Cell -> Html Msg
viewCell position cell =
    div [ class "cell", onClick (CellClicked position) ]
        [ text
            (cell
                |> Maybe.map toString
                |> Maybe.withDefault ""
            )
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "board" ]
        (positions
            |> List.map
                (\position ->
                    board
                        |> Dict.get position
                        |> viewCell position
                )
        )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [ onClick Reset ] [ text "Tic-Tac-Toe" ]
        , viewBoard model.board
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
