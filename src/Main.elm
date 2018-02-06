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


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type alias Model =
    Int


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Tic-Tac-Toe" ]
        , div [ class "board" ]
            (List.range 1 9
                |> List.map (\i -> div [ class "square" ] [ text (toString i) ])
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
