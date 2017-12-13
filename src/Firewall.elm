module Firewall exposing (Firewall, findUncaughtDelay, fromList, getSeverity, iterate, next)

import Array exposing (Array)


type Firewall
    = Firewall
        { depth : Int
        , catches : List ( Int, Int )
        , layers : Array Layer
        }


type Layer
    = Empty
    | Guarded Guard


type alias Guard =
    { range : Int
    , pos : Int
    , direction : Direction
    }


type Direction
    = Increasing
    | Decreasing


fromList : List ( Int, Int ) -> Firewall
fromList list =
    Firewall
        { depth = -1
        , catches = []
        , layers = makeLayers list
        }


makeLayers : List ( Int, Int ) -> Array Layer
makeLayers list =
    makeLayersHelper ( list, Array.empty )
        |> Tuple.second


makeLayersHelper :
    ( List ( Int, Int ), Array Layer )
    -> ( List ( Int, Int ), Array Layer )
makeLayersHelper ( list, array ) =
    case list of
        [] ->
            ( list, array )

        ( depth, range ) :: rest ->
            let
                length =
                    Array.length array

                paddingSize =
                    max 0 (depth - length)

                padding =
                    Array.repeat paddingSize Empty

                paddedArray =
                    Array.append array padding

                guard =
                    Guarded
                        { range = range
                        , pos = 0
                        , direction = Increasing
                        }

                newArray =
                    Array.push guard paddedArray
            in
            if depth < length then
                ( list, array )
            else
                makeLayersHelper ( rest, newArray )


next : Firewall -> Firewall
next firewall =
    let
        (Firewall { depth, catches, layers }) =
            firewall

        newDepth =
            depth + 1

        layer =
            Array.get newDepth layers |> Maybe.withDefault Empty

        severity =
            case layer of
                Empty ->
                    Nothing

                Guarded { range, pos } ->
                    if pos == 0 then
                        Just (newDepth * range)
                    else
                        Nothing

        newCatches =
            case severity of
                Just number ->
                    ( newDepth, number ) :: catches

                Nothing ->
                    catches

        newLayers =
            Array.map nextLayer layers
    in
    Firewall
        { depth = newDepth
        , catches = newCatches
        , layers = newLayers
        }


nextLayer : Layer -> Layer
nextLayer layer =
    case layer of
        Empty ->
            Empty

        Guarded { range, pos, direction } ->
            let
                ( newPos, newDirection ) =
                    case direction of
                        Increasing ->
                            if pos >= range - 1 then
                                ( range - 2, Decreasing )
                            else
                                ( pos + 1, Increasing )

                        Decreasing ->
                            if pos <= 0 then
                                ( 1, Increasing )
                            else
                                ( pos - 1, Decreasing )
            in
            Guarded
                { range = range
                , pos = max 0 (min (range - 1) newPos)
                , direction = newDirection
                }


iterate : Firewall -> Firewall
iterate firewall =
    let
        (Firewall { depth, layers }) =
            firewall
    in
    if depth >= Array.length layers then
        firewall
    else
        iterate (next firewall)


getsCaught : Firewall -> Bool
getsCaught firewall =
    let
        (Firewall { depth, catches, layers }) =
            firewall
    in
    if depth >= 0 && catches /= [] then
        True
    else if depth >= Array.length layers then
        False
    else
        getsCaught (next firewall)


getSeverity : Firewall -> Int
getSeverity (Firewall { catches }) =
    catches
        |> List.map Tuple.second
        |> List.sum


findUncaughtDelay : Firewall -> Int
findUncaughtDelay =
    findUncaughtDelayHelper 0


findUncaughtDelayHelper : Int -> Firewall -> Int
findUncaughtDelayHelper delay (Firewall firewallData) =
    let
        delayedFirewall =
            Firewall { firewallData | depth = firewallData.depth - delay }
    in
    if getsCaught delayedFirewall then
        findUncaughtDelayHelper (delay + 1) (Firewall firewallData)
    else
        delay
