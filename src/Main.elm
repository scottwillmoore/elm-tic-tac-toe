module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Player
    = X
    | O


type alias Position =
    ( Int, Int )


positions : List Position
positions =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]


type alias Board =
    { turn : Player
    , tiles : Dict Position Player
    }


type alias Model =
    Board


defaultModel : Model
defaultModel =
    { turn = O
    , tiles = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = Reset
    | Play Position
    | Fill Position Player


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            (defaultModel, Cmd.none)

        Play position ->
            let
                nextTurn = case model.turn of
                    X -> O
                    O -> X
            in
                update (Fill position nextTurn) { model | turn = nextTurn }

        Fill position player ->
            ( { model | tiles = Dict.insert position player model.tiles }, Cmd.none )


toDom : Dict Position Player -> Position -> Html Msg
toDom tiles position =
    let
        tile =
            Dict.get position tiles
    in
        case tile of
            Nothing ->
                div [ class "tile", onClick (Play position) ] []

            Just player ->
                div [ class "tile" ] [ text (toString player) ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [ onClick Reset ] [ text "Tic-Tac-Toe" ]
        , div [ class "board" ]
            (List.map (toDom model.tiles) positions)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
