module Board exposing (set, get, place, toList, toIndexedList, map, indexedMap, status)

import Dict exposing (Dict)
import Types exposing (..)


set : Board -> Player -> Position -> Board
set board player position =
    Dict.insert position player board


get : Board -> Position -> Cell
get board position =
    Dict.get position board


place : Board -> Player -> Position -> ( Board, Bool )
place board player position =
    case get board position of
        Just player ->
            ( board, False )

        Nothing ->
            ( set board player position, True )


toList : Board -> List Cell
toList board =
    positions |> List.map (get board)


toIndexedList : Board -> List ( Cell, Position )
toIndexedList board =
    positions |> List.map (\position -> ( get board position, position ))


map : (Cell -> a) -> Board -> List a
map f board =
    board |> toList |> List.map f


indexedMap : (Cell -> Position -> a) -> Board -> List a
indexedMap f board =
    board |> toIndexedList |> List.map (\tuple -> f (Tuple.first tuple) (Tuple.second tuple))


checkStraight : Board -> List Position -> Cell
checkStraight board straight =
    let
        cells =
            straight |> List.map (get board)
    in
        if List.all ((==) (Just O)) cells then
            Just O
        else if List.all ((==) (Just X)) cells then
            Just X
        else
            Nothing


status : Board -> Status
status board =
    let
        cells =
            straights
                |> List.map (checkStraight board)
    in
        if List.any ((==) (Just O)) cells then
            Win O
        else if List.any ((==) (Just X)) cells then
            Win X
        else if List.all ((/=) Nothing) (toList board) then
            Draw
        else
            Play


listProduct : List a -> List a -> List (List a)
listProduct xs ys =
    case xs of
        [] ->
            []

        z :: zs ->
            List.foldr (\a b -> [ a, z ] :: b) [] ys ++ listProduct zs ys


listToTuple2 : ( a, a ) -> List a -> ( a, a )
listToTuple2 default list =
    case list of
        [ a, b ] ->
            ( a, b )

        _ ->
            default


positions : List Position
positions =
    listProduct [ 1, 2, 3 ] [ 1, 2, 3 ]
        |> List.map (listToTuple2 ( 0, 0 ))


horizontal : Int -> List Position
horizontal n =
    positions |> List.filter (Tuple.second >> (==) n)


horizontals : List (List Position)
horizontals =
    List.range 1 3 |> List.map horizontal


vertical : Int -> List Position
vertical n =
    positions |> List.filter (Tuple.first >> (==) n)


verticals : List (List Position)
verticals =
    List.range 1 3 |> List.map vertical


majorDiagonal : List Position
majorDiagonal =
    positions
        |> List.filter
            (\position -> Tuple.first position == Tuple.second position)


minorDiagonal : List Position
minorDiagonal =
    positions
        |> List.filter
            (\position -> Tuple.first position == 4 - (Tuple.second position))


diagonals : List (List Position)
diagonals =
    [ majorDiagonal
    , minorDiagonal
    ]


straights : List (List Position)
straights =
    horizontals ++ verticals ++ diagonals
