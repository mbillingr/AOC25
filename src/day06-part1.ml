import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";


let parse_line = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.num, 0);
let parse_ops = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.any_char, 0);

// two definitions to avoid writing type annotations :)
let parse1 = fun parser -> vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parser);
let parse2 = fun parser -> vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parser);

let ws = parse1 parse_line;
let xs = parse1 parse_line;
let ys = parse1 parse_line;
let zs = parse1 parse_line;
let ops = parse2 parse_ops;
let tasks = iter.zip(ops, iter.zip(iter.zip(ws, xs), iter.zip(ys, zs)));

let compute = fun (op, ((w, x), (y, z))) -> 
  if op == "+" then w + x + y + z else
  if op == "*" then w * x * y * z else
  panic op;

let result = int.sum iter.map(compute, tasks);

io.write_line ("Day 6, Part 1: " ^ int.to_str(result));

