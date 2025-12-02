import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
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


let all_ids = iter.flatten(iter.map(iter.range, vec.iter ranges));


let is_invalid = fun id -> begin
  let chars = vec.collect str.chars (int.to_str id);
  let (lhs, rhs) = vec.split(chars, vec.length(chars)/2);
  vec.equal(lhs, rhs, (fun (a,b) -> a == b))
end;

let invalid_ids = iter.filter(is_invalid, all_ids);
let sum_invalid = iter.fold((fun (a,b) -> a + b), 0, invalid_ids);

io.write_line ("Day 2, Part 1: " ^ int.to_str(sum_invalid));

