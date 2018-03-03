module View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Types exposing (..)
import Board exposing (..)


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
