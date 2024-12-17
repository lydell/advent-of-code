import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import line_parser

type Op {
  Adv(Combo)
  Bxl(Literal)
  Bst(Combo)
  Jnz(Literal)
  Bxc
  Out(Combo)
  Bdv(Combo)
  Cdv(Combo)
}

type Combo {
  V0
  V1
  V2
  V3
  RA
  RB
  RC
}

type Literal {
  Literal(Int)
}

type Registers {
  Registers(a: Int, b: Int, c: Int)
}

pub fn main() {
  let #(registers, ops) =
    case line_parser.parse_stdin(Ok) {
      [
        "Register A: " <> a,
        "Register B: " <> b,
        "Register C: " <> c,
        "",
        "Program: " <> rest,
      ] -> {
        use a_int <- result.try(line_parser.parse_int("A", a))
        use b_int <- result.try(line_parser.parse_int("B", b))
        use c_int <- result.try(line_parser.parse_int("C", c))
        use pairs <- result.try(result.replace_error(
          make_pairs(string.split(rest, ",")),
          "Expected an even number of numbers",
        ))
        use ops <- result.map(line_parser.parse_general(
          pairs,
          "Pair",
          fn(tuple) { tuple.0 <> "," <> tuple.1 },
          parse_op,
        ))
        #(
          Registers(a: a_int, b: b_int, c: c_int),
          dict.from_list(list.index_map(ops, fn(op, index) { #(index, op) })),
        )
      }
      _ -> Error("Unexpected format")
    }
    |> line_parser.panic_on_error

  eval(registers, 0, ops)
  |> list.map(int.to_string)
  |> string.join(",")
  |> io.println

  Nil
}

fn make_pairs(items: List(a)) -> Result(List(#(a, a)), Nil) {
  case items {
    [] -> Ok([])
    [_] -> Error(Nil)
    [a, b, ..rest] -> {
      use tail <- result.map(make_pairs(rest))
      [#(a, b), ..tail]
    }
  }
}

fn parse_op(tuple: #(String, String)) -> Result(Op, String) {
  let combo = fn(constructor: fn(Combo) -> Op) -> Result(Op, String) {
    parse_combo(tuple.1)
    |> result.map(constructor)
  }

  let literal = fn(constructor: fn(Literal) -> Op) -> Result(Op, String) {
    parse_literal(tuple.1)
    |> result.map(constructor)
  }

  case tuple.0 {
    "0" -> combo(Adv)
    "1" -> literal(Bxl)
    "2" -> combo(Bst)
    "3" -> literal(Jnz)
    "4" -> Ok(Bxc)
    "5" -> combo(Out)
    "6" -> combo(Bdv)
    "7" -> combo(Cdv)
    other -> Error("Unknown opcode: " <> other)
  }
}

fn parse_combo(input: String) -> Result(Combo, String) {
  case input {
    "0" -> Ok(V0)
    "1" -> Ok(V1)
    "2" -> Ok(V2)
    "3" -> Ok(V3)
    "4" -> Ok(RA)
    "5" -> Ok(RB)
    "6" -> Ok(RC)
    "7" ->
      Error(
        "Combo operand 7 is reserved and should not appear in valid programs.",
      )
    _ -> Error("Unknown Combo operand: " <> input)
  }
}

fn parse_literal(input: String) -> Result(Literal, String) {
  case input {
    "0" -> Ok(Literal(0))
    "1" -> Ok(Literal(1))
    "2" -> Ok(Literal(2))
    "3" -> Ok(Literal(3))
    "4" -> Ok(Literal(4))
    "5" -> Ok(Literal(5))
    "6" -> Ok(Literal(6))
    "7" -> Ok(Literal(7))
    _ -> Error("Unknown Literal operand: " <> input)
  }
}

fn eval(registers: Registers, pointer: Int, ops: Dict(Int, Op)) -> List(Int) {
  case dict.get(ops, pointer) {
    Error(Nil) -> []
    Ok(op) ->
      case op {
        Adv(combo) -> {
          let numerator = registers.a
          let assert Ok(denominator) =
            int.power(2, int.to_float(get_combo_value(combo, registers)))
          let res = numerator / float.truncate(denominator)
          eval(Registers(..registers, a: res), pointer + 1, ops)
        }
        Bxl(Literal(literal)) -> {
          let res = int.bitwise_exclusive_or(registers.b, literal)
          eval(Registers(..registers, b: res), pointer + 1, ops)
        }
        Bst(combo) -> {
          let assert Ok(res) = int.modulo(get_combo_value(combo, registers), 8)
          eval(Registers(..registers, b: res), pointer + 1, ops)
        }
        Jnz(Literal(literal)) ->
          case registers.a {
            0 -> eval(registers, pointer + 1, ops)
            _ -> eval(registers, literal / 2, ops)
          }
        Bxc -> {
          let res = int.bitwise_exclusive_or(registers.b, registers.c)
          eval(Registers(..registers, b: res), pointer + 1, ops)
        }
        Out(combo) -> {
          let assert Ok(res) = int.modulo(get_combo_value(combo, registers), 8)
          [res, ..eval(registers, pointer + 1, ops)]
        }
        Bdv(combo) -> {
          let numerator = registers.a
          let assert Ok(denominator) =
            int.power(2, int.to_float(get_combo_value(combo, registers)))
          let res = numerator / float.truncate(denominator)
          eval(Registers(..registers, b: res), pointer + 1, ops)
        }
        Cdv(combo) -> {
          let numerator = registers.a
          let assert Ok(denominator) =
            int.power(2, int.to_float(get_combo_value(combo, registers)))
          let res = numerator / float.truncate(denominator)
          eval(Registers(..registers, c: res), pointer + 1, ops)
        }
      }
  }
}

fn get_combo_value(combo: Combo, registers: Registers) -> Int {
  case combo {
    V0 -> 0
    V1 -> 1
    V2 -> 2
    V3 -> 3
    RA -> registers.a
    RB -> registers.b
    RC -> registers.c
  }
}
