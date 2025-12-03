
import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/itertools.ml";
import "libs/set.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";



let input = io.lines() |> (option.unwrap_or "");

let parse_range = parsing.map_result(
  (fun xs -> (vec.peek(xs, 0) |> option.unwrap, vec.peek(xs, 1) |> option.unwrap)), 
  parsing.seplist(parsing.char "-", parsing.num, 2));

let ranges = parsing.parse(input, 
  parsing.seplist(parsing.char(","), parse_range, 0)) |> option.unwrap;

  
let invalid_in_range = fun(lo, hi, patrep) -> begin
  let n_digits_min = int.ceil_div(int.log(10, lo) + 1, patrep);
  let n_digits_max = int.floor_div(int.log(10, hi) + 1, patrep);
  let n_min = int.repeat(9, n_digits_min - 1) + 1;
  let n_max = int.repeat(9, n_digits_max);
  let pats = iter.range(n_min, n_max + 1);
  let invals = iter.map((fun p -> int.repeat(p, patrep)), pats);
  let above_lo = iter.filter((fun id -> id >= lo), invals);
  let below_hi = iter.take_while((fun id -> id <= hi), above_lo);
  below_hi
end;


let multi_invalid_in_range = fun(lo, hi) -> begin
  let max_digits = int.log(10, hi) + 1;
  iter.flatten iter.map((fun pr -> invalid_in_range(lo, hi, pr)), iter.range(2, max_digits+1))
end;


let part1 = set.collect iter.flatten iter.map((fun (lo, hi) -> invalid_in_range(lo, hi, 2)), vec.iter_pbar ranges);
let part1 = int.sum set.iter part1;

io.write_line ("Day 2, Part 1: " ^ int.to_str(part1));

let part2 =  set.collect iter.flatten iter.map((fun (lo, hi) -> multi_invalid_in_range(lo, hi)), vec.iter_pbar ranges);
let part2 = int.sum set.iter part2;

io.write_line ("Day 2, Part 1: " ^ int.to_str(part2));

