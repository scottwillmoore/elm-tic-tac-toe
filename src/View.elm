module View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Board exposing (..)


none : Html Msg
none =
    text ""


viewPlayer : Player -> Html Msg
viewPlayer player =
    case player of
        O ->
            div [ class "player player-o" ] [ text (toString player) ]

        X ->
            div [ class "player player-x" ] [ text (toString player) ]


viewCell : Cell -> Position -> Html Msg
viewCell cell position =
    div [ class "cell", onClick (CellClicked position) ]
        [ cell
            |> Maybe.map viewPlayer
            |> Maybe.withDefault none
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    div [ class "board" ] (board |> Board.indexedMap viewCell)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [ onClick Reset ] [ text "Tic-Tac-Toe" ]
        , viewBoard model.board
        , p [] [ text (toString model.status) ]
        ]
