module View exposing (view)

import Html exposing (Html, text, div, span, h1, p)
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


viewScore : ( Int, Int ) -> Html Msg
viewScore ( o, x ) =
    div [ class "score" ]
        [ span [ class "o" ] [ text (toString o) ]
        , span [] [ text " - " ]
        , span [ class "x" ] [ text (toString x) ]
        ]


viewResult : Cell -> Html Msg
viewResult cell =
    div [ class "result" ] [ viewPlayer (Maybe.withDefault X cell) ]


viewHistory : List Cell -> Html Msg
viewHistory history =
    div [ class "history" ] (history |> List.map viewResult)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "aside" ]
            [ viewScore ( 1, 1 ) -- TODO: remove score?
            , viewHistory model.history
            ]
        , viewBoard model.board
        ]
