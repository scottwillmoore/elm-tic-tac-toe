module View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Board exposing (..)


viewCell : Cell -> Position -> Html Msg
viewCell cell position =
    div [ class "cell", onClick (CellClicked position) ]
        [ text
            (cell
                |> Maybe.map toString
                |> Maybe.withDefault ""
            )
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "board" ] (board |> Board.indexedMap viewCell)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [ onClick Reset ] [ text "Tic-Tac-Toe" ]
        , viewBoard model.board
        , div [] [ text (toString model.status) ]
        ]
