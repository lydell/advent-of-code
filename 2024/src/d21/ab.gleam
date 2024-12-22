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
    keypad_robots: Int,
    presses: Int,
    code: List(Numpad),
  )
}

pub fn main() {
  let codes =
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

  let part1 =
    codes
    |> list.map(run(_, 2))
    |> int.sum

  let part2 =
    codes
    |> list.map(run(_, 25))
    |> int.sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn run(code: List(Numpad), keypad_robots: Int) -> Int {
  // The codes always end with A, so we don't need to keep state between codes.
  let #(state, _) =
    go(State(numpad_robot: NA, keypad_robots:, presses: 0, code:), dict.new())

  let assert Ok(numeric_part_of_code) =
    code
    |> list.filter_map(numpad_to_int)
    |> list.map(int.to_string)
    |> string.join("")
    |> int.parse

  state.presses * numeric_part_of_code
}

type Cache =
  Dict(#(Keypad, Keypad, Int, Int), Int)

fn go(state: State, cache: Cache) -> #(State, Cache) {
  case state.code {
    [] -> #(state, cache)
    [num, ..rest] -> {
      let #(p, cache) = case
        presses_needed_for_numpad(state.numpad_robot, num)
      {
        NoPressesNeeded -> press_a(#(0, cache), KA, 1, state.keypad_robots)
        OneDirection(#(key, times)) ->
          presses(KA, key, times, state.keypad_robots, cache)
          |> press_a(key, 1, state.keypad_robots)
        TwoDirections(first, second) ->
          presses_twice(KA, first, second, state.keypad_robots, cache)
          |> press_a(second.0, 1, state.keypad_robots)
        TwoDirectionsReversible(first, second) -> {
          let #(a, cache_a) =
            presses_twice(KA, first, second, state.keypad_robots, cache)
            |> press_a(second.0, 1, state.keypad_robots)
          let #(b, cache_b) =
            presses_twice(KA, second, first, state.keypad_robots, cache)
            |> press_a(first.0, 1, state.keypad_robots)
          case a < b {
            True -> #(a, cache_a)
            False -> #(b, cache_b)
          }
        }
      }
      go(
        State(
          numpad_robot: num,
          keypad_robots: state.keypad_robots,
          presses: state.presses + p,
          code: rest,
        ),
        cache,
      )
    }
  }
}

fn presses(
  current_keypad_position: Keypad,
  wanted_keypad_position: Keypad,
  press_times: Int,
  remaining_keypads: Int,
  cache: Cache,
) -> #(Int, Cache) {
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
        0 -> #(press_times, cache)
        _ -> {
          let #(p, cache) = case
            presses_needed_for_keypad(
              current_keypad_position,
              wanted_keypad_position,
            )
          {
            NoPressesNeeded ->
              press_a(#(0, cache), KA, 1, remaining_keypads - 1)
            OneDirection(#(key, times)) ->
              presses(KA, key, times, remaining_keypads - 1, cache)
              |> press_a(key, press_times, remaining_keypads - 1)
            TwoDirections(first, second) ->
              presses_twice(KA, first, second, remaining_keypads - 1, cache)
              |> press_a(second.0, press_times, remaining_keypads - 1)
            TwoDirectionsReversible(first, second) -> {
              let #(a, cache_a) =
                presses_twice(KA, first, second, remaining_keypads - 1, cache)
                |> press_a(second.0, press_times, remaining_keypads - 1)

              let #(b, cache_b) =
                presses_twice(KA, second, first, remaining_keypads - 1, cache)
                |> press_a(first.0, press_times, remaining_keypads - 1)

              case a < b {
                True -> #(a, cache_a)
                False -> #(b, cache_b)
              }
            }
          }
          #(p, cache)
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
  remaining_keypads: Int,
  cache: Cache,
) -> #(Int, Cache) {
  let #(p1, cache) = presses(next, first.0, first.1, remaining_keypads, cache)
  let #(p2, cache) =
    presses(first.0, second.0, second.1, remaining_keypads, cache)
  #(p1 + p2, cache)
}

fn press_a(
  result: #(Int, Cache),
  key: Keypad,
  times: Int,
  remaining_keypads: Int,
) -> #(Int, Cache) {
  let #(p1, cache) = result
  let #(p2, cache) = presses(key, KA, times, remaining_keypads, cache)
  #(p1 + p2, cache)
}

type PressesNeeded {
  NoPressesNeeded
  OneDirection(#(Keypad, Int))
  TwoDirections(#(Keypad, Int), #(Keypad, Int))
  TwoDirectionsReversible(#(Keypad, Int), #(Keypad, Int))
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
  case from {
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
}

//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
fn presses_needed_for_keypad(from: Keypad, to: Keypad) -> PressesNeeded {
  case from {
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
