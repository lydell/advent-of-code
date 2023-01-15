module Matrix.Extra exposing (toMatrix)

import Array exposing (Array)
import Matrix exposing (Matrix)
import Set


toMatrix : a -> List (List a) -> Result String (Matrix a)
toMatrix default list =
    let
        array =
            list
                |> List.map Array.fromList
                |> Array.fromList

        widths =
            list |> List.map List.length |> Set.fromList

        width =
            case Set.toList widths of
                [] ->
                    Ok 0

                [ single ] ->
                    Ok single

                _ ->
                    Err
                        ("Sublists have different lengths: "
                            ++ (widths |> Set.toList |> List.map String.fromInt |> String.join ", ")
                        )
    in
    width |> Result.map (toMatrixHelper default array)


toMatrixHelper : a -> Array (Array a) -> Int -> Matrix a
toMatrixHelper default array width =
    Matrix.generate
        width
        (Array.length array)
        (\x y ->
            Array.get y array
                |> Maybe.andThen (Array.get x)
                |> Maybe.withDefault default
        )
