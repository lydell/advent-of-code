// Too high: 161952
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
    robot1: Numpad,
    robot2: Keypad,
    robot3: Keypad,
    presses: List(Keypad),
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
    let min_presses = case
      go(State(robot1: NA, robot2: KA, robot3: KA, presses: [], code:))
    {
      [] -> panic as "empty list of ways to do the code"
      [first, ..rest] ->
        list.fold(rest, list.length(first.presses), fn(min, state) {
          int.min(min, list.length(state.presses))
        })
    }

    let assert Ok(numeric_part_of_code) =
      code
      |> list.filter_map(numpad_to_int)
      |> list.map(int.to_string)
      |> string.join("")
      |> int.parse

    min_presses * numeric_part_of_code
  })
  |> int.sum
  |> io.debug

  Nil
}

fn go(state: State) -> List(State) {
  case state.code {
    [] -> [state]
    [num, ..rest] -> {
      // Move robot 1 into place.
      let states = case presses_needed_for_numpad(state.robot1, num) {
        NoPressesNeeded -> [state]
        OneDirection(#(key, times)) -> press_key_on_robot2(state, key, times)
        TwoDirections(#(key1, times1), #(key2, times2)) ->
          state
          |> press_key_on_robot2(key1, times1)
          |> list.flat_map(press_key_on_robot2(_, key2, times2))
        TwoDirectionsReversible(#(key1, times1), #(key2, times2)) ->
          list.append(
            state
              |> press_key_on_robot2(key1, times1)
              |> list.flat_map(press_key_on_robot2(_, key2, times2)),
            state
              |> press_key_on_robot2(key2, times2)
              |> list.flat_map(press_key_on_robot2(_, key1, times1)),
          )
      }

      // Activate robot 1.
      let states =
        list.flat_map(states, fn(state) {
          press_key_on_robot2(State(..state, robot1: num), KA, 1)
        })

      // Press next part of the code.
      list.flat_map(states, fn(state) { go(State(..state, code: rest)) })
    }
  }
}

fn press_key_on_robot2(state: State, key: Keypad, times: Int) -> List(State) {
  // Move robot 2 in place.
  let states = case presses_needed_for_keypad(state.robot2, key) {
    NoPressesNeeded -> [state]
    OneDirection(#(key, times)) -> [press_key_on_robot3(state, key, times)]
    TwoDirections(#(key1, times1), #(key2, times2)) -> [
      state
      |> press_key_on_robot3(key1, times1)
      |> press_key_on_robot3(key2, times2),
    ]
    TwoDirectionsReversible(#(key1, times1), #(key2, times2)) -> [
      state
        |> press_key_on_robot3(key1, times1)
        |> press_key_on_robot3(key2, times2),
      state
        |> press_key_on_robot3(key2, times2)
        |> press_key_on_robot3(key1, times1),
    ]
  }

  // Activate robot 2.
  list.map(states, fn(state) {
    press_key_on_robot3(State(..state, robot2: key), KA, times)
  })
}

fn press_key_on_robot3(state: State, key: Keypad, times: Int) -> State {
  // Move robot 3 in place.
  let presses = case presses_needed_for_keypad(state.robot3, key) {
    NoPressesNeeded -> []
    OneDirection(#(next_key, next_times)) -> list.repeat(next_key, next_times)
    TwoDirections(#(key1, times1), #(key2, times2)) ->
      list.append(list.repeat(key1, times1), list.repeat(key2, times2))
    TwoDirectionsReversible(#(key1, times1), #(key2, times2)) ->
      // Both forwards and backwards are equivalent when the human presses the button (no further dependency).
      list.append(list.repeat(key1, times1), list.repeat(key2, times2))
  }

  // Activate robot 3.
  State(
    ..state,
    robot3: key,
    presses: list.append(
      state.presses,
      list.append(presses, list.repeat(KA, times)),
    ),
  )
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
        N2 -> TwoDirections(#(KUp, 1), #(KLeft, 1))
        N3 -> OneDirection(#(KUp, 1))
        N4 -> TwoDirections(#(KUp, 2), #(KLeft, 2))
        N5 -> TwoDirections(#(KUp, 2), #(KLeft, 1))
        N6 -> OneDirection(#(KUp, 2))
        N7 -> TwoDirections(#(KUp, 3), #(KLeft, 2))
        N8 -> TwoDirections(#(KUp, 3), #(KLeft, 1))
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