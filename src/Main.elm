module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Tile
    = X
    | O
    | Blank


toString : Tile -> String
toString tile =
    case tile of
        X ->
            "X"

        O ->
            "O"

        Blank ->
            ""


type alias Model =
    { squares : List Tile
    }


defaultModel : Model
defaultModel =
    { squares = List.range 1 9 |> List.map (\i -> Blank)
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , div [ class "board" ]
            (model.squares
                |> List.map (\i -> div [ class "tile" ] [ text (toString i) ])
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
