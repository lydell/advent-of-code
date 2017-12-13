module Firewall exposing (fromList, getSeverity, iterate, next)

import Array exposing (Array)


type Firewall
    = Firewall
        { depth : Int
        , severity : Int
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
        , severity = 0
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
        (Firewall { depth, severity, layers }) =
            firewall

        newDepth =
            depth + 1

        layer =
            Array.get newDepth layers |> Maybe.withDefault Empty

        layerSeverity =
            case layer of
                Empty ->
                    0

                Guarded { range, pos } ->
                    if pos == 0 then
                        newDepth * range
                    else
                        0

        newLayers =
            Array.map nextLayer layers
    in
    Firewall
        { depth = newDepth
        , severity = severity + layerSeverity
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


getSeverity : Firewall -> Int
getSeverity (Firewall { severity }) =
    severity
