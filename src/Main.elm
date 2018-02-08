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
    { tiles : Dict Position Player
    }


type alias Model =
    Board


defaultModel : Model
defaultModel =
    { tiles = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = Fill Position Player


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                div [ class "tile", onClick (Fill position X) ] []

            Just player ->
                div [ class "tile" ] [ text (toString player) ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , div [ class "board" ]
            (List.map (toDom model.tiles) positions)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
