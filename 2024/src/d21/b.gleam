// Too high: 161952
import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import line_parser

type Numpad {
  NA
  N0
  N1
  N2
  N3
  N4
  N5
  N6
  N7
  N8
  N9
}

type Keypad {
  KLeft
  KDown
  KRight
  KUp
  KA
}

type State {
  State(
    numpad_robot: Numpad,
    keypad_robots: List(Keypad),
    presses: Int,
    code: List(Numpad),
  )
}

pub fn main() {
  line_parser.parse_stdin(fn(line) {
    line_parser.parse_general(
      string.to_graphemes(line),
      "Char",
      function.identity,
      fn(char) {
        case char {
          "0" -> Ok(N0)
          "1" -> Ok(N1)
          "2" -> Ok(N2)
          "3" -> Ok(N3)
          "4" -> Ok(N4)
          "5" -> Ok(N5)
          "6" -> Ok(N6)
          "7" -> Ok(N7)
          "8" -> Ok(N8)
          "9" -> Ok(N9)
          "A" -> Ok(NA)
          _ -> Error("Unknown char")
        }
      },
    )
  })
  |> list.map(fn(code) {
    // The codes always end with A, so we don't need to keep state between codes.
    let #(state, _) =
      go(
        State(
          numpad_robot: NA,
          keypad_robots: list.repeat(KA, 2),
          presses: 0,
          code:,
        ),
        dict.new(),
      )

    let min_presses = state.presses

    let assert Ok(numeric_part_of_code) =
      code
      |> list.filter_map(numpad_to_int)
      |> list.map(int.to_string)
      |> string.join("")
      |> int.parse

    io.debug(#(code, min_presses, numeric_part_of_code))

    min_presses * numeric_part_of_code
  })
  |> int.sum
  |> io.debug

  Nil
}

type Cache =
  Dict(#(Keypad, Keypad, Int, List(Keypad)), #(Int, List(Keypad)))

fn go(state: State, cache: Cache) -> #(State, Cache) {
  case state.code {
    [] -> #(state, cache)
    [num, ..rest] ->
      case state.keypad_robots {
        [] -> panic as "no keypad robots"
        [first_keypad_robot, ..rest_keypad_robots] -> {
          // Move the numpad robot into place.
          let #(#(#(p1, rest_keypad_robots), cache), first_keypad_robot) = case
            presses_needed_for_numpad(state.numpad_robot, num)
          {
            NoPressesNeeded -> #(
              #(#(0, state.keypad_robots), cache),
              first_keypad_robot,
            )
            OneDirection(#(key, times)) -> #(
              presses(first_keypad_robot, key, times, rest_keypad_robots, cache),
              key,
            )
            TwoDirections(first, second) -> #(
              presses_twice(
                first_keypad_robot,
                first,
                second,
                rest_keypad_robots,
                cache,
              ),
              second.0,
            )
            TwoDirectionsReversible(first, second) -> {
              let #(a, cache_a) =
                presses_twice(
                  first_keypad_robot,
                  first,
                  second,
                  rest_keypad_robots,
                  cache,
                )
              let #(b, cache_b) =
                presses_twice(
                  first_keypad_robot,
                  second,
                  first,
                  rest_keypad_robots,
                  cache,
                )
              case a.0 < b.0 {
                True -> #(#(a, cache_a), second.0)
                False -> #(#(b, cache_b), first.0)
              }
            }
          }

          // Activate the numpad robot.
          let #(#(p2, rest_keypad_robots), cache) =
            presses(first_keypad_robot, KA, 1, rest_keypad_robots, cache)

          // Press next part of the code.
          go(
            State(
              numpad_robot: num,
              keypad_robots: [KA, ..rest_keypad_robots],
              presses: state.presses + p1 + p2,
              code: rest,
            ),
            cache,
          )
        }
      }
  }
}

fn presses(
  current_keypad_position: Keypad,
  wanted_keypad_position: Keypad,
  press_times: Int,
  remaining_keypads: List(Keypad),
  cache: Cache,
) -> #(#(Int, List(Keypad)), Cache) {
  let cache_key = #(
    current_keypad_position,
    wanted_keypad_position,
    press_times,
    remaining_keypads,
  )
  case dict.get(cache, cache_key) {
    Ok(cached) -> #(cached, cache)
    Error(Nil) -> {
      let #(result, cache) = case remaining_keypads {
        [] -> #(#(press_times, []), cache)
        [next, ..rest] -> {
          let #(#(#(p1, rest), cache), next) = case
            presses_needed_for_keypad(
              current_keypad_position,
              wanted_keypad_position,
            )
          {
            NoPressesNeeded -> #(#(#(0, rest), cache), next)
            OneDirection(#(key, times)) -> #(
              presses(next, key, times, rest, cache),
              key,
            )
            TwoDirections(first, second) -> #(
              presses_twice(next, first, second, rest, cache),
              second.0,
            )
            TwoDirectionsReversible(first, second) -> {
              let #(a, cache_a) =
                presses_twice(next, first, second, rest, cache)

              let #(b, cache_b) =
                presses_twice(next, second, first, rest, cache)

              case a.0 < b.0 {
                True -> #(#(a, cache_a), second.0)
                False -> #(#(b, cache_b), first.0)
              }
            }
          }
          let #(#(p2, rest), cache) =
            presses(next, KA, press_times, rest, cache)
          #(#(p1 + p2, [KA, ..rest]), cache)
        }
      }
      #(result, dict.insert(cache, cache_key, result))
    }
  }
}

fn presses_twice(
  next: Keypad,
  first: #(Keypad, Int),
  second: #(Keypad, Int),
  remaining_keypads: List(Keypad),
  cache: Cache,
) -> #(#(Int, List(Keypad)), Cache) {
  let #(#(p1, remaining_keypads), cache) =
    presses(next, first.0, first.1, remaining_keypads, cache)
  let #(#(p2, remaining_keypads), cache) =
    presses(first.0, second.0, second.1, remaining_keypads, cache)
  #(#(p1 + p2, remaining_keypads), cache)
}

type PressesNeeded {
  NoPressesNeeded
  OneDirection(#(Keypad, Int))
  TwoDirections(#(Keypad, Int), #(Keypad, Int))
  TwoDirectionsReversible(#(Keypad, Int), #(Keypad, Int))
}

fn numpad_grid() {
  [
    #(#(0, 0), N7),
    #(#(1, 0), N8),
    #(#(2, 0), N9),
    #(#(0, 1), N4),
    #(#(1, 1), N5),
    #(#(2, 1), N6),
    #(#(0, 2), N1),
    #(#(1, 2), N2),
    #(#(2, 2), N3),
    #(#(1, 3), N0),
    #(#(2, 3), NA),
  ]
}

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+
fn presses_needed_for_numpad(from: Numpad, to: Numpad) -> PressesNeeded {
  let result = case from {
    NA ->
      case to {
        NA -> NoPressesNeeded
        N0 -> OneDirection(#(KLeft, 1))
        N1 -> TwoDirections(#(KUp, 1), #(KLeft, 2))
        N2 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        N3 -> OneDirection(#(KUp, 1))
        N4 -> TwoDirections(#(KUp, 2), #(KLeft, 2))
        N5 -> TwoDirections(#(KUp, 2), #(KLeft, 1))
        N6 -> OneDirection(#(KUp, 2))
        N7 -> TwoDirections(#(KUp, 3), #(KLeft, 2))
        N8 -> TwoDirectionsReversible(#(KUp, 3), #(KLeft, 1))
        N9 -> OneDirection(#(KUp, 3))
      }
    N0 ->
      case to {
        NA -> OneDirection(#(KRight, 1))
        N0 -> NoPressesNeeded
        N1 -> TwoDirections(#(KUp, 1), #(KLeft, 1))
        N2 -> OneDirection(#(KUp, 1))
        N3 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
        N4 -> TwoDirections(#(KUp, 2), #(KLeft, 1))
        N5 -> OneDirection(#(KUp, 2))
        N6 -> TwoDirectionsReversible(#(KUp, 2), #(KRight, 1))
        N7 -> TwoDirections(#(KUp, 3), #(KLeft, 1))
        N8 -> OneDirection(#(KUp, 3))
        N9 -> TwoDirectionsReversible(#(KUp, 3), #(KRight, 1))
      }
    N1 ->
      case to {
        NA -> TwoDirections(#(KRight, 2), #(KDown, 1))
        N0 -> TwoDirections(#(KRight, 1), #(KDown, 1))
        N1 -> NoPressesNeeded
        N2 -> OneDirection(#(KRight, 1))
        N3 -> OneDirection(#(KRight, 2))
        N4 -> OneDirection(#(KUp, 1))
        N5 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
        N6 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 2))
        N7 -> OneDirection(#(KUp, 2))
        N8 -> TwoDirectionsReversible(#(KUp, 2), #(KRight, 1))
        N9 -> TwoDirectionsReversible(#(KUp, 2), #(KRight, 2))
      }
    N2 ->
      case to {
        NA -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        N0 -> OneDirection(#(KDown, 1))
        N1 -> OneDirection(#(KLeft, 1))
        N2 -> NoPressesNeeded
        N3 -> OneDirection(#(KRight, 1))
        N4 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        N5 -> OneDirection(#(KUp, 1))
        N6 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
        N7 -> TwoDirectionsReversible(#(KUp, 2), #(KLeft, 1))
        N8 -> OneDirection(#(KUp, 2))
        N9 -> TwoDirectionsReversible(#(KUp, 2), #(KRight, 1))
      }
    N3 ->
      case to {
        NA -> OneDirection(#(KDown, 1))
        N0 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        N1 -> OneDirection(#(KLeft, 2))
        N2 -> OneDirection(#(KLeft, 1))
        N3 -> NoPressesNeeded
        N4 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 2))
        N5 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        N6 -> OneDirection(#(KUp, 1))
        N7 -> TwoDirectionsReversible(#(KUp, 2), #(KLeft, 2))
        N8 -> TwoDirectionsReversible(#(KUp, 2), #(KLeft, 1))
        N9 -> OneDirection(#(KUp, 2))
      }
    N4 ->
      case to {
        NA -> TwoDirections(#(KRight, 2), #(KDown, 2))
        N0 -> TwoDirections(#(KRight, 1), #(KDown, 2))
        N1 -> OneDirection(#(KDown, 1))
        N2 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        N3 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 2))
        N4 -> NoPressesNeeded
        N5 -> OneDirection(#(KRight, 1))
        N6 -> OneDirection(#(KRight, 2))
        N7 -> OneDirection(#(KUp, 1))
        N8 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
        N9 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 2))
      }
    N5 ->
      case to {
        NA -> TwoDirectionsReversible(#(KDown, 2), #(KRight, 1))
        N0 -> OneDirection(#(KDown, 2))
        N1 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        N2 -> OneDirection(#(KDown, 1))
        N3 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        N4 -> OneDirection(#(KLeft, 1))
        N5 -> NoPressesNeeded
        N6 -> OneDirection(#(KRight, 1))
        N7 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        N8 -> OneDirection(#(KUp, 1))
        N9 -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
      }
    N6 ->
      case to {
        NA -> OneDirection(#(KDown, 2))
        N0 -> TwoDirectionsReversible(#(KDown, 2), #(KLeft, 1))
        N1 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 2))
        N2 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        N3 -> OneDirection(#(KDown, 1))
        N4 -> OneDirection(#(KLeft, 2))
        N5 -> OneDirection(#(KLeft, 1))
        N6 -> NoPressesNeeded
        N7 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 2))
        N8 -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        N9 -> OneDirection(#(KUp, 1))
      }
    N7 ->
      case to {
        NA -> TwoDirections(#(KRight, 2), #(KDown, 3))
        N0 -> TwoDirections(#(KRight, 1), #(KDown, 3))
        N1 -> OneDirection(#(KDown, 2))
        N2 -> TwoDirectionsReversible(#(KDown, 2), #(KRight, 1))
        N3 -> TwoDirectionsReversible(#(KDown, 2), #(KRight, 2))
        N4 -> OneDirection(#(KDown, 1))
        N5 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        N6 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 2))
        N7 -> NoPressesNeeded
        N8 -> OneDirection(#(KRight, 1))
        N9 -> OneDirection(#(KRight, 2))
      }
    N8 ->
      case to {
        NA -> TwoDirectionsReversible(#(KDown, 3), #(KRight, 1))
        N0 -> OneDirection(#(KDown, 3))
        N1 -> TwoDirectionsReversible(#(KDown, 2), #(KLeft, 1))
        N2 -> OneDirection(#(KDown, 2))
        N3 -> TwoDirectionsReversible(#(KDown, 2), #(KRight, 1))
        N4 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        N5 -> OneDirection(#(KDown, 1))
        N6 -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        N7 -> OneDirection(#(KLeft, 1))
        N8 -> NoPressesNeeded
        N9 -> OneDirection(#(KRight, 1))
      }
    N9 ->
      case to {
        NA -> OneDirection(#(KDown, 3))
        N0 -> TwoDirectionsReversible(#(KDown, 3), #(KLeft, 1))
        N1 -> TwoDirectionsReversible(#(KDown, 2), #(KLeft, 2))
        N2 -> TwoDirectionsReversible(#(KDown, 2), #(KLeft, 1))
        N3 -> OneDirection(#(KDown, 2))
        N4 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 2))
        N5 -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        N6 -> OneDirection(#(KDown, 1))
        N7 -> OneDirection(#(KLeft, 2))
        N8 -> OneDirection(#(KLeft, 1))
        N9 -> NoPressesNeeded
      }
  }

  let grid = numpad_grid()

  let assert Ok(#(from_position, _)) =
    grid
    |> list.find(fn(tuple) { tuple.1 == from })

  let to_position = case result {
    NoPressesNeeded -> from_position
    OneDirection(#(key, times)) -> apply_key(from_position, key, times)
    TwoDirections(#(key1, times1), #(key2, times2)) -> {
      let corner = from_position |> apply_key(key2, times2)
      let _ = case grid |> list.find(fn(tuple) { tuple.0 == corner }) {
        Error(Nil) -> Nil
        Ok(_) -> {
          io.println(
            "from: " <> string.inspect(from) <> ", to: " <> string.inspect(to),
          )
          panic as "should have been TwoDirectionsReversible"
        }
      }
      from_position |> apply_key(key1, times1) |> apply_key(key2, times2)
    }
    TwoDirectionsReversible(#(key1, times1), #(key2, times2)) ->
      from_position |> apply_key(key1, times1) |> apply_key(key2, times2)
  }

  let assert Ok(#(_, to2)) =
    grid
    |> list.find(fn(tuple) { tuple.0 == to_position })

  case to == to2 {
    True -> result
    False -> panic as "got em"
  }
}

fn apply_key(position: #(Int, Int), key: Keypad, times: Int) -> #(Int, Int) {
  let #(x, y) = position
  case key {
    KA -> panic as "apply_key A"
    KDown -> #(x, y + times)
    KLeft -> #(x - times, y)
    KRight -> #(x + times, y)
    KUp -> #(x, y - times)
  }
}

fn keypad_grid() {
  [
    #(#(1, 0), KUp),
    #(#(2, 0), KA),
    #(#(0, 1), KLeft),
    #(#(1, 1), KDown),
    #(#(2, 1), KRight),
  ]
}

//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
fn presses_needed_for_keypad(from: Keypad, to: Keypad) -> PressesNeeded {
  let result = case from {
    KLeft ->
      case to {
        KLeft -> NoPressesNeeded
        KDown -> OneDirection(#(KRight, 1))
        KRight -> OneDirection(#(KRight, 2))
        KUp -> TwoDirections(#(KRight, 1), #(KUp, 1))
        KA -> TwoDirections(#(KRight, 2), #(KUp, 1))
      }
    KDown ->
      case to {
        KLeft -> OneDirection(#(KLeft, 1))
        KDown -> NoPressesNeeded
        KRight -> OneDirection(#(KRight, 1))
        KUp -> OneDirection(#(KUp, 1))
        KA -> TwoDirectionsReversible(#(KUp, 1), #(KRight, 1))
      }
    KRight ->
      case to {
        KLeft -> OneDirection(#(KLeft, 2))
        KDown -> OneDirection(#(KLeft, 1))
        KRight -> NoPressesNeeded
        KUp -> TwoDirectionsReversible(#(KUp, 1), #(KLeft, 1))
        KA -> OneDirection(#(KUp, 1))
      }
    KUp ->
      case to {
        KLeft -> TwoDirections(#(KDown, 1), #(KLeft, 1))
        KDown -> OneDirection(#(KDown, 1))
        KRight -> TwoDirectionsReversible(#(KDown, 1), #(KRight, 1))
        KUp -> NoPressesNeeded
        KA -> OneDirection(#(KRight, 1))
      }
    KA ->
      case to {
        KLeft -> TwoDirections(#(KDown, 1), #(KLeft, 2))
        KDown -> TwoDirectionsReversible(#(KDown, 1), #(KLeft, 1))
        KRight -> OneDirection(#(KDown, 1))
        KUp -> OneDirection(#(KLeft, 1))
        KA -> NoPressesNeeded
      }
  }

  let grid = keypad_grid()

  let assert Ok(#(from_position, _)) =
    grid
    |> list.find(fn(tuple) { tuple.1 == from })

  let to_position = case result {
    NoPressesNeeded -> from_position
    OneDirection(#(key, times)) -> apply_key(from_position, key, times)
    TwoDirections(#(key1, times1), #(key2, times2)) ->
      from_position |> apply_key(key1, times1) |> apply_key(key2, times2)
    TwoDirectionsReversible(#(key1, times1), #(key2, times2)) ->
      from_position |> apply_key(key1, times1) |> apply_key(key2, times2)
  }

  let assert Ok(#(_, to2)) =
    grid
    |> list.find(fn(tuple) { tuple.0 == to_position })

  case to == to2 {
    True -> result
    False -> panic as "got em"
  }
}

fn numpad_to_int(num: Numpad) -> Result(Int, Nil) {
  case num {
    NA -> Error(Nil)
    N0 -> Ok(0)
    N1 -> Ok(1)
    N2 -> Ok(2)
    N3 -> Ok(3)
    N4 -> Ok(4)
    N5 -> Ok(5)
    N6 -> Ok(6)
    N7 -> Ok(7)
    N8 -> Ok(8)
    N9 -> Ok(9)
  }
}
