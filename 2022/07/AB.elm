module AB exposing (..)

{-| To run, use the Elm repl:

    ❯ elm repl
    > import AB exposing (..)
    > import Input
    > solveA Input.input
    1749646 : Int
    > solveB Input.input
    1498966 : Int

-}

import Dict exposing (Dict)


type alias FileSystem =
    Dict PathReverse (List Node)


type alias PathReverse =
    List String


type Node
    = File String Int
    | Directory PathReverse


parse : String -> FileSystem
parse input =
    input
        |> String.trim
        |> String.lines
        |> List.map String.words
        |> List.foldl fold ( Dict.empty, [] )
        |> Tuple.first


fold : List String -> ( FileSystem, PathReverse ) -> ( FileSystem, PathReverse )
fold line ( fileSystem, cwd ) =
    case line of
        [ "$", "cd", "/" ] ->
            ( fileSystem, [] )

        [ "$", "cd", ".." ] ->
            ( fileSystem, List.drop 1 cwd )

        [ "$", "cd", directory ] ->
            ( fileSystem, directory :: cwd )

        [ "$", "ls" ] ->
            ( fileSystem, cwd )

        [ "dir", directory ] ->
            ( Dict.update cwd (addNode (Directory (directory :: cwd))) fileSystem, cwd )

        [ sizeString, filename ] ->
            ( Dict.update cwd (addNode (File filename (toInt sizeString))) fileSystem, cwd )

        _ ->
            Debug.todo ("Unknown line: " ++ Debug.toString line)


addNode : Node -> Maybe (List Node) -> Maybe (List Node)
addNode node maybePrevious =
    case maybePrevious of
        Just previous ->
            Just (node :: previous)

        Nothing ->
            Just [ node ]


toInt : String -> Int
toInt string =
    case String.toInt string of
        Just int ->
            int

        Nothing ->
            Debug.todo ("Invalid int: " ++ string)


{-| dus = du -s :)
-}
dus : FileSystem -> ( Dict PathReverse Int, Int )
dus fileSystem =
    dusHelper fileSystem [] Dict.empty


dusHelper : FileSystem -> PathReverse -> Dict PathReverse Int -> ( Dict PathReverse Int, Int )
dusHelper fileSystem cwd acc =
    case Dict.get cwd fileSystem of
        Just nodes ->
            let
                ( finalAcc, fullSum ) =
                    nodes
                        |> List.foldl
                            (\node ( nextAcc, sum ) ->
                                case node of
                                    File _ size ->
                                        ( nextAcc, sum + size )

                                    Directory path ->
                                        dusHelper fileSystem path nextAcc
                                            |> Tuple.mapSecond ((+) sum)
                            )
                            ( acc, 0 )
            in
            ( Dict.insert cwd fullSum finalAcc, fullSum )

        Nothing ->
            Debug.todo ("Directory not found (reverse): " ++ Debug.toString cwd)


solveA : String -> Int
solveA input =
    dus (parse input)
        |> Tuple.first
        |> Dict.values
        |> List.filter (\size -> size <= 100000)
        |> List.sum


diskSize : number
diskSize =
    70000000


neededUnusedSize : number
neededUnusedSize =
    30000000


solveB : String -> Int
solveB input =
    let
        ( sizes, totalSize ) =
            dus (parse input)

        needToFree =
            neededUnusedSize - (diskSize - totalSize)

        candidate =
            sizes
                |> Dict.values
                |> List.filter (\size -> size >= needToFree)
                |> List.minimum
    in
    case candidate of
        Just size ->
            size

        Nothing ->
            Debug.todo "No candidate found!"
