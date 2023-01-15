module AddingSpiral exposing (AddingSpiral, empty, getNumber, next, toListOfLists)

import Array
import Matrix exposing (Matrix)
import Matrix.Extra


type Pos
    = Right Int
    | Top Int
    | Left Int
    | Bottom Int


type AddingSpiral
    = AddingSpiral
        { matrix : Matrix Int
        , pos : Pos
        }


empty : AddingSpiral
empty =
    AddingSpiral
        { matrix = Matrix.repeat 1 1 1
        , pos = Bottom 0
        }


next : AddingSpiral -> AddingSpiral
next =
    nextPos >> updateNumber


nextPos : AddingSpiral -> AddingSpiral
nextPos (AddingSpiral { matrix, pos }) =
    let
        width =
            Matrix.width matrix

        height =
            Matrix.height matrix
    in
    case pos of
        Right i ->
            if i >= height - 1 then
                AddingSpiral
                    { matrix =
                        Matrix.concatVertical
                            (Matrix.repeat width 1 0)
                            matrix
                            |> Maybe.withDefault matrix
                    , pos = Top 0
                    }
            else
                AddingSpiral
                    { matrix = matrix
                    , pos = Right (i + 1)
                    }

        Top i ->
            if i >= width - 1 then
                AddingSpiral
                    { matrix =
                        Matrix.concatHorizontal
                            (Matrix.repeat 1 height 0)
                            matrix
                            |> Maybe.withDefault matrix
                    , pos = Left 0
                    }
            else
                AddingSpiral
                    { matrix = matrix
                    , pos = Top (i + 1)
                    }

        Left i ->
            if i >= height - 1 then
                AddingSpiral
                    { matrix =
                        Matrix.concatVertical
                            matrix
                            (Matrix.repeat width 1 0)
                            |> Maybe.withDefault matrix
                    , pos = Bottom 0
                    }
            else
                AddingSpiral
                    { matrix = matrix
                    , pos = Left (i + 1)
                    }

        Bottom i ->
            if i >= height - 1 then
                AddingSpiral
                    { matrix =
                        Matrix.concatHorizontal
                            matrix
                            (Matrix.repeat 1 height 0)
                            |> Maybe.withDefault matrix
                    , pos = Right 0
                    }
            else
                AddingSpiral
                    { matrix = matrix
                    , pos = Bottom (i + 1)
                    }


updateNumber : AddingSpiral -> AddingSpiral
updateNumber addingSpiral =
    let
        (AddingSpiral { matrix, pos }) =
            addingSpiral

        ( x, y ) =
            getCoords addingSpiral

        number =
            Matrix.Extra.neighbours x y matrix
                |> List.sum
    in
    AddingSpiral
        { matrix = Matrix.set x y number matrix
        , pos = pos
        }


getNumber : AddingSpiral -> Int
getNumber addingSpiral =
    let
        (AddingSpiral { matrix }) =
            addingSpiral

        ( x, y ) =
            getCoords addingSpiral
    in
    Matrix.get x y matrix |> Maybe.withDefault 0


getCoords : AddingSpiral -> ( Int, Int )
getCoords (AddingSpiral { matrix, pos }) =
    let
        width =
            Matrix.width matrix

        height =
            Matrix.height matrix
    in
    case pos of
        Right i ->
            ( width - 1, height - 1 - i )

        Top i ->
            ( width - 1 - i, 0 )

        Left i ->
            ( 0, i )

        Bottom i ->
            ( i, height - 1 )


toListOfLists : AddingSpiral -> List (List Int)
toListOfLists (AddingSpiral { matrix }) =
    List.range 0 (Matrix.height matrix - 1)
        |> List.filterMap (\y -> Matrix.getRow y matrix)
        |> List.map Array.toList
