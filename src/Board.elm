module Board exposing (positions, straights)

import Types exposing (..)


product : List a -> List a -> List (List a)
product xs ys =
    case xs of
        [] ->
            []

        z :: zs ->
            List.foldr (\a b -> [ a, z ] :: b) [] ys ++ product zs ys


tuple2 : ( a, a ) -> List a -> ( a, a )
tuple2 default list =
    case list of
        [ a, b ] ->
            ( a, b )

        _ ->
            default


positions : List Position
positions =
    product [ 1, 2, 3 ] [ 1, 2, 3 ]
        |> List.map (tuple2 ( 0, 0 ))


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
