port module Runner exposing (..)

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Json.Decode


type alias Model =
    ()


type Msg
    = Input String


port input : (String -> msg) -> Sub msg


port output : ( Bool, String ) -> Cmd msg


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subcriptions
        }


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


subcriptions : Model -> Sub Msg
subcriptions model =
    input Input


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input day ->
            case run day of
                Ok ( result1, result2 ) ->
                    ( (), output ( True, "Result 1: " ++ result1 ++ "\nResult 2: " ++ result2 ) )

                Err message ->
                    ( (), output ( False, message ) )


run : String -> Result String ( String, String )
run day =
    case day of
        "1" ->
            Ok (Day1.output ())

        "2" ->
            Ok (Day2.output ())

        "3" ->
            Ok (Day3.output ())

        "4" ->
            Ok (Day4.output ())

        "5" ->
            Ok (Day5.output ())

        "6" ->
            Ok (Day6.output ())

        _ ->
            Err ("Invalid day value. See the end of src/Runner.elm for valid ones. Got: " ++ day)
