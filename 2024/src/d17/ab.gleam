import gleam/dict.{type Dict}
import gleam/float
import gleam/function
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
  let #(registers, ops, numbers) =
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
        use numbers <- result.try(
          line_parser.parse_general(
            string.split(rest, ","),
            "Number",
            function.identity,
            line_parser.parse_int("Number", _),
          ),
        )
        use pairs <- result.try(result.replace_error(
          make_pairs(numbers),
          "Expected an even number of numbers",
        ))
        use ops <- result.map(line_parser.parse_general(
          pairs,
          "Pair",
          fn(tuple) { int.to_string(tuple.0) <> "," <> int.to_string(tuple.1) },
          parse_op,
        ))
        #(
          Registers(a: a_int, b: b_int, c: c_int),
          dict.from_list(list.index_map(ops, fn(op, index) { #(index, op) })),
          numbers,
        )
      }
      _ -> Error("Unexpected format")
    }
    |> line_parser.panic_on_error

  let part1 =
    eval(registers, 0, ops)
    |> list.map(int.to_string)
    |> string.join(",")

  // My input ends with adv(3), out(b), jnz(0).
  // That `adv` is the _only_ thing that mutates the A register.
  // That `out` is the _only_ op that outputs.
  // That `jnz` is the _only_ jump.
  // Then my input translates to:
  //
  // while A != 0:
  //   mutate B and C a bunch
  //   A = A // 2**3
  //   out(B % 8)
  //
  // Since the jump is at the end, and it only jumps if a isn’t 0,
  // and we know that A doesn’t start out as 0, and it is the only jump,
  // it is effectively a while loop, where A becomes smaller (divided by 8, the only mutation)
  // each iteration until it becomes 0.
  //
  // Since there is only a single `out`, and it prints based on the register B,
  // we only need to consider what it takes for it to output a wanted number.
  //
  // `mutate B and C a bunch` set both register B and C based on register A,
  // so no state is carried between iterations.
  //
  // Since register A is divided by 8 each time, A must be 0-8 in order to output just one number.
  // Since the iterations are independent, an A of 0-8 must output the last wanted number.
  // By running with A set to each of 0-8, I noticed that just one of the results was the wanted value (0).
  // Then I knew what A had to be at the last iteration. That was when A=1.
  //
  // What does it take from the iteration before to get A=1 on the last iteration?
  // Well, 8-15 all become 1 when divided by 8 and truncated. So it has to be one of those.
  // When I ran with A set to each of 8-15, the same thing happened: Only one of the results was the
  // last two wanted values (3,0). That was at A=10.
  //
  // What does it take from the iteration before that to get A=10? 80-87 all become 10 when divided
  // by 8 and truncated. In other words, for a working A value on one iteration, I need to try
  // from A*8 to (A+1)*8-1 in the previous iteration.
  //
  // And so on. After a while, more than one result matched, so then I had to try both of those
  // (a list of matching results, not just one). In the end, only a single A value was found.
  // (a list with one item).
  //
  // Note: This only works with my specific input. But I think that it can be tweaked for solve other
  // people’s inputs – I guess all have similar characteristics with just minor differences.
  //
  // Note 2: Since the part 2 is input specific, it would have been a lot easier to just hard code
  // part 1 too (hand translate the ops to code), instead of parsing the input and implement the whole interpreter.
  // But it was fun at least!
  let part2 = case list.reverse(numbers) {
    [] -> panic as "numbers is empty"
    [last, ..rest] ->
      search(registers, ops, 0, 7, rest, [last])
      |> list.map(int.to_string)
      |> string.join(" OR ")
  }

  io.println("Part 1: " <> part1)
  io.println("Part 2: " <> part2)
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

fn parse_op(tuple: #(Int, Int)) -> Result(Op, String) {
  let combo = fn(constructor: fn(Combo) -> Op) -> Result(Op, String) {
    parse_combo(tuple.1)
    |> result.map(constructor)
  }

  let literal = fn(constructor: fn(Literal) -> Op) -> Result(Op, String) {
    parse_literal(tuple.1)
    |> result.map(constructor)
  }

  case tuple.0 {
    0 -> combo(Adv)
    1 -> literal(Bxl)
    2 -> combo(Bst)
    3 -> literal(Jnz)
    4 -> Ok(Bxc)
    5 -> combo(Out)
    6 -> combo(Bdv)
    7 -> combo(Cdv)
    other -> Error("Unknown opcode: " <> int.to_string(other))
  }
}

fn parse_combo(input: Int) -> Result(Combo, String) {
  case input {
    0 -> Ok(V0)
    1 -> Ok(V1)
    2 -> Ok(V2)
    3 -> Ok(V3)
    4 -> Ok(RA)
    5 -> Ok(RB)
    6 -> Ok(RC)
    7 ->
      Error(
        "Combo operand 7 is reserved and should not appear in valid programs.",
      )
    _ -> Error("Unknown Combo operand: " <> int.to_string(input))
  }
}

fn parse_literal(input: Int) -> Result(Literal, String) {
  case input <= 7 {
    True -> Ok(Literal(input))
    False -> Error("Unknown Literal operand: " <> int.to_string(input))
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

fn search(registers, ops, lower, upper, reversed_key, acc) {
  let alternatives =
    list.range(lower, upper)
    |> list.filter(fn(a) { eval(Registers(..registers, a:), 0, ops) == acc })
  case reversed_key {
    [] -> alternatives
    [next, ..rest] ->
      alternatives
      |> list.flat_map(fn(a) {
        // See the comment at `let part2` for how `lower` and `upper` are calculated here.
        search(registers, ops, a * 8, { a + 1 } * 8 - 1, rest, [next, ..acc])
      })
  }
}
