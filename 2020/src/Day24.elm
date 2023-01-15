module Day24 exposing (..)

import Hexagons.Hex as Hex exposing (Direction, Hex)
import Hexagons.Map exposing (Hash)
import Html exposing (Html)
import LineParser
import Regex exposing (Regex)
import Set exposing (Set)
import Svg.Attributes exposing (x)


regex : Regex
regex =
    Regex.fromString "e|se|sw|w|nw|ne|."
        |> Maybe.withDefault Regex.never


parse : String -> Result String (List (List Direction))
parse =
    LineParser.parse
        (Regex.find regex
            >> List.map .match
            >> LineParser.parseGeneral "Direction"
                identity
                (\direction ->
                    case direction of
                        "e" ->
                            Ok Hex.E

                        "se" ->
                            Ok Hex.SE

                        "sw" ->
                            Ok Hex.SW

                        "w" ->
                            Ok Hex.W

                        "nw" ->
                            Ok Hex.NW

                        "ne" ->
                            Ok Hex.NE

                        _ ->
                            Err ("Unknown direction: " ++ direction)
                )
        )


solution1 : String -> Result String Int
solution1 =
    parse >> Result.map (solve1 >> Set.size)


solve1 : List (List Direction) -> Set Hash
solve1 =
    List.foldl
        (\directions blacks ->
            let
                next =
                    List.foldl
                        (\direction hex ->
                            Hex.neighbor hex direction
                        )
                        (Hex.intFactory ( 0, 0 ))
                        directions
                        |> Hexagons.Map.hashHex
            in
            if Set.member next blacks then
                Set.remove next blacks

            else
                Set.insert next blacks
        )
        Set.empty


solution2 : String -> Result String Int
solution2 =
    parse >> Result.map (solve2 >> Set.size)


solve2 : List (List Direction) -> Set Hash
solve2 lists =
    List.repeat 100 ()
        |> List.foldl
            (always flip)
            (solve1 lists)


flip : Set Hash -> Set Hash
flip inputBlacks =
    inputBlacks
        |> Set.toList
        |> List.foldl
            (\hash ( seenWhites, blacks ) ->
                let
                    neighbors =
                        getAllNeighbors (Hex.IntCubeHex hash)

                    blackNeighbors =
                        neighbors
                            |> List.filter (\neighbor -> Set.member neighbor inputBlacks)
                            |> List.length

                    nextBlacks =
                        if blackNeighbors == 0 || blackNeighbors > 2 then
                            blacks

                        else
                            Set.insert hash blacks
                in
                neighbors
                    |> List.filter
                        (\neighbor ->
                            not (Set.member neighbor inputBlacks)
                                && not (Set.member neighbor seenWhites)
                        )
                    |> List.foldl
                        (\whiteHash ( whites_, blacks_ ) ->
                            let
                                blackNeighbors_ =
                                    getAllNeighbors (Hex.IntCubeHex whiteHash)
                                        |> List.filter (\neighbor -> Set.member neighbor inputBlacks)
                                        |> List.length
                            in
                            ( Set.insert whiteHash whites_
                            , if blackNeighbors_ == 2 then
                                Set.insert whiteHash blacks_

                              else
                                blacks_
                            )
                        )
                        ( seenWhites, nextBlacks )
            )
            ( Set.empty, Set.empty )
        |> Tuple.second


getAllNeighbors : Hex -> List Hash
getAllNeighbors hex =
    [ Hex.NE
    , Hex.E
    , Hex.SE
    , Hex.SW
    , Hex.W
    , Hex.NW
    ]
        |> List.map (Hex.neighbor hex >> Hexagons.Map.hashHex)


main : Html Never
main =
    Html.div []
        [ showResult (solution1 puzzleInput)
        , showResult (solution2 puzzleInput)
        ]


showResult : Result String Int -> Html msg
showResult result =
    Html.output []
        [ Html.text
            (case result of
                Ok int ->
                    String.fromInt int

                Err error ->
                    error
            )
        ]


puzzleInput : String
puzzleInput =
    """
weseswseswesewseseseswewswseswnenwsese
nwswwwnwwwswswseswseswwsewswnewsw
wnwnwsewwnwenwnwnwwwnwnwnewnwswne
nwnwnwewnwnwswwnenwsenenwnwnwnwenwnwnw
swswewswewneseeseswseewwsewnwne
swwnenenenwswsweneswswswswneewwsewnw
neswewseswswseswswswswneswwswsenewsese
nwwnenenwwnwnewnwsenenwsenwse
eswwnwswswswwneswwwwewnwnwswsewse
sewseneeesesewseseswnesesewseseeese
nwweneseeswnesewsesenwsese
swswswneswswswswwsw
nwnwnwnwnwswnwwnwnwnenwewnwsenwnwese
enwwneneseseswewneeswsenwneswneseenw
enwswesenwnwseenwseseeeswnwnwneseswnwe
eswwwswneneesenwenenenwswswnese
wnwweseswnwneenenewswwnweswwsesw
nwneswneeeswewwneseneseswswwwsenenw
nwweweeeswnwswesweneenweneeew
swseswswwwseswwwwwwwwnwewwne
nwsewsenwnwwwnenw
neswswneeeeeeneenwwnwneewseswwse
seesesewseswenwsesewneeenenwswnesew
eeenweeesweeeee
swnwnwnwswsewnenewswwwweswwsewswe
seneenwnwwnwswwnwnwnwwnwnwewwnwnw
swswneenenwwewswnwwswnewwnwswwww
neswenenwnwnenenenesenwswswnweswwsewnenw
eeseeeenweeseesesenwe
nenwneneneewneesenesenenenene
eenesewseseewe
nwnwwwwwwswswwneweeewswwwww
wwnesenweseneneeeneswswswnesenwnwe
swseseswenwsesesenweeswsesesewswnwnwnw
newswnweswswseswnenenene
neneeneneeeneeeswneeneesewwnwsenw
neeneswwnwnenwwneenwneseswneenewe
nenwswseswneswswswneswswswswwsewsweseww
sesewseeseesesesenewsenwnwswewsenew
swswneeseswswwwwswwneswswnwswswswsenw
swswswseswswneswswseswseswsweww
swwneswneswwseseenwswswswswswsenwswswsw
seswseesenwewsesesw
sweneswnwenewneseneenenwnwneseswseenw
nwnenenwnwnewenenwsweswne
wwnewwwwwwewewnwwwwnesewse
nwnwsewnwnwweswnenwnw
swneeneneeneneneneeenenee
eneweseeesenwwweeeeeeweene
nenwnwswnwnwnwnwnwnwnwnwnwnenwnwsese
nwneneesweneeeneenenene
eneesweneneenenwneesenwneneeneswnw
seswnwswneswswnwswswseseseswsesenwseesese
swwenwenwnwnwnwnwswwnwneswwnwneeenw
wseeseneneswwwnewnenenenwswwswswwsw
eneenwnwnenwwnwnwnwnwnwswnwnwnwnwsew
nwnwswswnwnwnwnwweweenwwnwnwnew
swenwsenenwswwneenenenwswnenenenenwnenenw
swnenwwneseswswwswneswwnewswswesenw
nenenenenwnwnesenwswnwenenwnenwenwneswne
wenenweeswsweswnwseswneeneneswswnw
seesesesesweseneswswswwnwwswsenewswsw
newwwsenwnwswwwnweswnwnewnwwsenese
senwwnwnenwnenenwnenenwnweenwnewwnw
eneswwseenenwnenwswswseeneenesesew
eseeeswwenwenweswseneeenenenene
seeswnweeswneeeeneswneneneneeeew
sewswwswwswneswseswneswnenwwswswseww
seseenwseswsesenwnwnwwseseesesese
swswseneswneswswswswsweswswswwswswnwsw
neeseeseneneenweewe
swwsenenenenwneneneneneneneeneneneswne
enwnenwnwneewswseenenenwswswnwnwenwnwsw
neswenwneeeeeeee
neesenesewswnwswsesewswseswsese
nwnwnwnewsenwnwsenesenwneswnewnenwnenw
eneswsenenenewene
wsenwnwnenwnwswwnwnwenwnenwswsenewwnw
nenewswneswsenwenwnwnesenwnwneeswneene
swneseseneswnesenesewwswesewseseswse
eneeewnewwwswnewesenwseseweswsene
neesweneswswneeeswneneeeeneenenw
wsenwnwwnwnwnwnwwwwww
sewwsewswwwnwwnwenwwnewwnwnwe
neeneeneeswneeeneee
swseneseneseseswnenwswneswenenwseswww
seseswswswwsewswswnewsesweneswnwswesw
seswswsenwswneswswswnwwsewnwwwswwse
swsenwswnwwswswnwsenwneneenesenwnenee
swnwswsewswswnewswswswswseswswswseswe
neeeeneneneneneweeene
sesweneseswnwneswswnwwnwneewenesee
sewswsesenesewseseseneswseseswnesesesw
nwswwswwswswswswswwneswswwswe
enwnewnwswnenenwwseswnwswsenwnenwene
esesesenweseeseseseeenwseseseswsenw
neeneneneswseewneswnwnenwnwnesenenesee
swswwwswswswswesww
wwwswnwswswenesewenweewneenwene
eseeenewwwwnenenewwneneneseewsese
wswsesweswswswnewswnw
neeeeneenesweeeswswneeeenwsenw
sesesenewnwswsenwnwnwwnwnwwwwseswsese
seeesesweeeseeenwenwsweneseewee
wneweswswwneenwwwsenewsewwwsw
nenewneneneneneenenenesewnenenene
seswswnwnenwnwnenwnenenenewnesweeswse
swesesesewnwseesenwswnwe
swewswwesesewswwnwweneenwswsww
seeswewswnwseeseseseesenwnwnwenwseese
neneswswwwseneswwneswswswswwswsesesw
seenwweeswenesweneeeneeeenweene
swwesenwswwenenwneewwsesww
nwwnwneeenwswnwenwnewwwnwnwswnwnw
neswneneneneneeneeenwneneswnenewnesene
eseseenesenwswsesenwseseeseseeswnese
neseswswenwnewneneswnenwneseswsweswsww
swsenwseneswswnesweeswswswswswswswswsww
eseeewseeeseneseeeseeewneseww
nwnwswwnwnwnwnwnenwwww
wwwwwnwnwwwneseww
seeswseneseeeseseseswwneeeseesesenw
wnwewnwnwwwnesewwnwnw
eesewsenwsesenesee
sesesewesweneewnweneesesese
nwseewseneswseseseeseewsenenweswese
wneswwswnenwwswseeswwwnw
nesesesewneseswswwswswswswesenwnwnwsesw
nwnwnenwnwnwnewnwwswnwewnwnwswenwnw
swseeswwnwnwsenenenwswwswseseswswsesesesw
swseseseeneewsesesewneesewseseeese
sewwsesenwwewnenenwneneenewnwnwe
nwwenwwnwnenwnenwnwnwsene
nwenwnwewnwwesesenwswwnwnenwwwne
senwsewwnwwwwwseenwswwnwwneese
wwwswwnwswsewewewwnewewswsww
swneeeenenwnwneswese
neswwwwwnweseenwneenwswswseeswnw
eswnenwswneeeswseneswneenenwnenwee
nwnewneswswwneeseseseswseneeneeseesw
neeneeeeweneswenweseswee
eseswnwwswnwsweseseswseswswsesesweswsw
seenwenwseswsesewweseseseweswse
eenweseswewneswwnwwenwwwenwsw
swseseswwwswwnenewwswswnwnesesw
wwnwswwswsewneneewnwnwwswnwwwwse
nenenesenenwnwnenenw
nenwwneneseneneswneswnenenweneneswne
nwsenewwnwnwnewwwswswswseeswwwswsee
swnwnwnwswneseeenesweeewesweeeee
swseseenwswswswwnwswwswsweswse
nwswnenenwneswswnenenwswnwneswswnweneneene
swswenwsenesewswswnwwenwswnwneswswswsw
wwswswnwwneswswswneeswsw
nwsweswwwwswwswwnewswweswsewswne
seewseswswnwwswsenenesweeswswwswnwnenw
neenwswseeswswsweseeneneeneewsewe
weenwsesesenwnwseseswseseseswnesesee
seswswseswswswnewsesw
nwnwnwwnwnwnwnwnwenwnwnwnw
neswneswseneswneneneneneneneewneneenw
swwnwneswnwswswwswswswsesewneeswesee
nenwnewnesenwswnwnenwnenwe
esenwswsesenesewseswseseswseswsw
eseweeeeewneeeeeeewene
seswnwneswswwseswsewsweseswswswseswse
nwwnwewwnwwnwswnww
nwseseswseswswswswswsweswswsw
enesenesenewswsenwneswnenwnewnwnwnenwnw
seswswnwneswsweswswswswswnwswswweswswesw
wseswswwwwsenenesweseeseswse
nwnwnewwenwnwswseew
nwwnwsenwnwnwnenesenwnenwnwnenwnwnwnwse
wwnwsewnewwsewnwnenwsenwwwwnwww
wwwewwwwwwwwwwwswnwe
seseesweenwenwsenwswsenwseeeseseswese
swwenwnwnwnwnwnwwnwnwnww
nwnenwneneseneneneswenenenenwnwsenwnew
nenwsenwnenenesenenenenenenwnwnwneseww
nwwnwnenenesenenenesenenwnenenenesewe
neneewneswnwsewnenenwnene
nenwneewswnwnwneneewneneneenwnenenw
nwwwnwwnwnwseswnwwwsenwwneswnewewne
esweneenenwsewwswswswswwseswsewnwne
wseenwseswnesenwse
eeeswenwesenesenwewswne
neswseseseneswseswseseneswseseswsenesesese
wsewewwnewwnenwsenewwwwswswsw
wneeeswewsesenenwsewnwsewnewseese
swnweewnwwseeswnenwseswneswwnesese
sesenwewsesewswsweneneseeseeesene
wnwenwwwnwewsewwwwswsewwww
seneenenwnweeeeeeneeseenwnwswse
wsenewwwwwnwwwnwnwnww
nenwnwenwwwnenewwneenenwesenene
seeeswsweseesewenweeseseeneenee
sesewenwseeeeseewswnweeeseeenw
swenwnwswnwnwseeneswnwnene
swswswwswneesewswwwneeneeweswsw
swwnweeswnesenenweseseeesesewseese
sweseswwwswwneenwwwswe
wneneneeesenenenwneseeneneneeneswne
swwesesewneswwenwwswwwwwwwwnwsw
swenwswswswswswwwwnwnwnwswewswswee
senwswnwseeeneeswse
swswnwesweswswsweswswswwswnwnwne
neswneeneneneneneneeneene
eneseeseeswenweeweseseswseeenese
seswnwseseswswenesenenesewsewswswswse
wsesenwneseseseew
swwwswwewswwswsww
nwswswnwswswsweswsweswswewsenwswnene
eeeeeewnweeeweeseeswnwneseee
nwwwnwnwwsenwnenwswenwnwnwnwnwnenwnwe
seswsenwnwwseeseseneeswswswswswswnenw
swwneeeeenenweswe
wwwwnwwwnwswnenwsewwewww
swnenwnenwnwnewswnwnwenenw
seewswsesenwswneeee
nwwwewwwenwwwwweswwwnwnwnwse
wwnwwwswnwewwwswwswnwenewww
seswnenwneswnwsweenwnweenwwwweswnw
neswneseenesweenenenew
nenenwwneneneeeneneenenwswewesweene
swswswswswneswswswnwswseswswwneeswesw
seeweeeeseseeneeswnwewnwesee
swnwewswswnwswneswswswsesweneswswswsw
swnewneswnwswseswswswswsweswswswneswsw
swnwswneneneewnwwenwse
sewwwnewwswwwwwsw
swnwswnewnwsesewsewnenwnwsenwsenwenw
nwnwnwnwsenwsenwnwnwnwnenwnenwnwswneswnwnw
senesesesesesesewsenwswseseneseeseswwse
sesenenweneseseseseswwnewseewsenww
wneneswnenenenenenesenenenenenenesenwnw
swswneenwnwnwnwwnwnwnwnwnenwnwsenwsenwnw
wwnwwneswswwwswswswswswesew
wwnwseswnwnewwswwwewsewwsewene
wswseeswswswswswnwwneeswswswwwswswsw
wwnewesesewwseenwnwswnwnwnwnwnwse
neeneswewwswneswnenenenweenewnenwsee
swenwnwsesweenweeeneneneswewseese
sewwseweseseeseseneseesesenwnewse
nenenwnwneswneneneneseeneneneswnenene
newseeeneneeeeneeneenenw
neeneneswswnwnwneneneneseneesewnwnene
seseenwseseeswseneseswsenwseesesesee
swswswsesenwswnwswseswneswseswswswseseswnw
eneneneneenwesweweeeeswwenese
enenwweeneneneneswswnenenenenenenene
sewswswnwswswneseseneneswswseswseswswsw
sesesenwwnwnwwnwwwwenenwwnww
seseseswswswseenwseseswsweseenwnwsesw
wnwswseswsenwsesenwnwesesweseesenenese
sesesenwseseseswseseseneseesewseswnww
seswseeseswnwnwseewenwswseeseswseswse
seseswseneswneenwwseesenewswnwswwswse
wnwneneseswnenewnwnenwneneenenenewsenene
neneseseeeewwewsweeenwnwneswee
nwwwewwewww
newnwswwesesewnenwneswnewnwseneesew
nwwwwewewwswwnenwsenwnwnwnwwww
neseneswnenwweeneseewswseeneeew
newwsenwsesesesweseewseseseneseseew
neneneswewneneneneeeewnenesene
enenwswseeneseseeseweseee
wnenewwswewseeswswwnwwswwwwsw
seswnwnweenwsweenwneseeneeenenwneswe
newwwwswswneswwwswsww
wwwwsenwswwnwewneswwww
neswnenwnenenwnenwswnenenwswnenwnw
wsesenenwnenwnwnwnwneswwnenenwnwnwene
neswswwwwnewswswwswswwnwsenewnwswee
eswwsenenwneeeseseweeeswswenese
newseenenweesweeseesewsesenwseesw
neswswsenesweseswwwwswsweeswsenwswsw
swnwneswnenewenenenenenwenwneneenesw
esweswswnwswswsewnweeewwwseeswnw
nwnwwseweswnwswnwwewnesewwnenenwnw
senwnwsenwnwnwnwseswwnwnenwnwnwnwenenw
swnewswwwwwnewwwwnwneewwwsew
neneneeeseneneneesewswewneeenee
wswsenewneenesewnenwnenwsenwneswswnene
neewwnwnenwnenesesenenwnenwsenenwneswnee
seswnwneenesenwnenenwswnewneneneenee
nwnwwnenwswwwnenwnwnwneswnwswnwnwsenwnwne
swwwnewsenesenwsenewseeswnenesenwwsw
senwseseseneswseenwwwse
seseswseseseseseseswswseenwswsesenw
nenesenenwswwsewnwswnwneneeewnwene
swswswwswswswswswswwneswsw
sweneseesenwesewnewnwseeseswnwesese
nwnwswswswseswswsweswsw
eswswwswswwnwswwswwswswneswswnwsesw
newwwwwwwewwsenwnenwewsenenwsw
nweswweenwnwswnwenwwnwswnwnwnwnwnwnw
senwwswnenwseswswswnewneeswwwww
senwseswswnweneeswseswswwseswneswswswsw
weswwsewnweeewweneswnesewsene
neeswswnwnwnwswnwswneewnwnwnwnwswnenww
nwswwneewnesenesenenwnenenenwnwnwnwnenw
sweseeneseeneswenwneswswnwesee
nesewnwnwwnwnwnwnwnwnwnwnenwnwsenwnw
newneeenweeeeneneseene
wesesenwneenenwneswwneneenweswsene
eswnwswnwsenesweneswswsweswswsenwswsesw
swsesenwseswswseswswewsenwswsesene
weseeseeswswweseeneseseneeseenwne
wswswswswwsenwswnewneswneswswwsewwsw
eseswnwesenwnwwnwswnwsweneeeneesesew
wnwwswswwwnewswsenwswswsesweswnww
eeeneeneswewseeeneeweneesww
wnenwswwseewwswnewwnewwsenwnwsew
nenwswenenwsenenwsenwnwnwnenewnenenwswsw
neswnewweeeeeeeneneenenenenwsee
ewnweeeeswswseeeenweeeeenwe

"""
