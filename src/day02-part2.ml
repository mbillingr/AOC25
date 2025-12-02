import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/itertools.ml";
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
let all_ids = vec.iter_pbar vec.collect all_ids;


let is_invalid = fun id -> begin
  let chars = vec.collect str.chars (int.to_str id);

  let patlens = iter.range(1, 1 + vec.length(chars)/2);
  let chunked = iter.map((fun pl -> itertools.chunks(pl, vec.iter chars)), patlens);
  
  iter.any(
    (fun chunks -> 
      iter.all(
        (fun (a, b) -> vec.equal(a, b, (fun (x,y) -> x == y))), 
        iter.sliding_pair chunks)),
    chunked)
end;

let invalid_ids = iter.filter(is_invalid, all_ids);
let sum_invalid = iter.fold((fun (a,b) -> a + b), 0, invalid_ids);

io.write_line ("Day 2, Part 1: " ^ int.to_str(sum_invalid));

