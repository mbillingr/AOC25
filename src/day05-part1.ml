import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";


let parse_range = parsing.map_result(
  (fun xs -> (vec.peek(xs, 0) |> option.unwrap, vec.peek(xs, 1) |> option.unwrap)), 
  parsing.seplist(parsing.char "-", parsing.num, 2));

let ranges = iter.map((fun line -> parsing.parse(line, parse_range)), io.lines);
let ranges = vec.collect iter.map(option.unwrap[a=(int*int)], iter.take_while(option.is_positive, ranges));

let ids = iter.map((fun line -> parsing.parse(line, parsing.num)), io.lines);
let ids = vec.collect iter.map(option.unwrap[a=int], ids);


let id_in_range = fun(id, (low, high)) -> (low <= id) && (id <= high);

let is_fresh = fun id -> iter.any((fun r -> id_in_range(id, r)), vec.iter ranges);

let fresh_ids = iter.filter(is_fresh, vec.iter ids);

let result = iter.count fresh_ids;

io.write_line ("Day 5, Part 1: " ^ int.to_str(result));

