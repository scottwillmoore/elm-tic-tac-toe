module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


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
    div []
        [ toString model |> text
        , button [ onClick Increment ] [ text "Increment" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
