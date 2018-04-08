module View exposing (view)

import Html exposing (Html, Attribute, text, div, span, h1, p)
import Html.Keyed
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Svg exposing (svg, circle, line)
import Svg.Attributes exposing (viewBox, cx, cy, r, x1, y1, x2, y2, fill, strokeWidth)
import Types exposing (..)
import Board exposing (..)


none : Html Msg
none =
    text ""


class2 : String -> Svg.Attribute msg
class2 msg =
    Svg.Attributes.class msg


kdiv : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
kdiv attributes children =
    Html.Keyed.node "div" attributes children


viewPlayer : Player -> Html Msg
viewPlayer player =
    case player of
        O ->
            svg [ class2 "player o", viewBox "0 0 10 10" ]
                [ circle [ cx "5", cy "5", r "4", fill "none", strokeWidth "0.4" ] []
                ]

        X ->
            svg [ class2 "player x", viewBox "0 0 10 10" ]
                [ line [ x1 "1", y1 "1", x2 "9", y2 "9", strokeWidth "0.4" ] []
                , line [ x1 "9", y1 "1", x2 "1", y2 "9", strokeWidth "0.4" ] []
                ]


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


viewStatus : Status -> List (Html Msg)
viewStatus status =
    case status of
        Play ->
            []

        Draw ->
            [ p [] [ text "Draw" ]
            , div [] [ viewPlayer O, viewPlayer X ]
            ]

        Win player ->
            [ p [] [ text "Win" ]
            , div [] [ viewPlayer player ]
            ]


viewResult : Int -> Status -> ( String, Html Msg )
viewResult index status =
    ( toString index, div [ class "result" ] (viewStatus status) )


viewHistory : List Status -> Html Msg
viewHistory history =
    kdiv [ class "history" ]
        (history
            |> List.reverse
            |> List.indexedMap viewResult
            |> List.reverse
        )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "aside" ] [ viewHistory model.history ]
        , viewBoard model.board
        ]
