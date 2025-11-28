import "libs/io.ml";
import "libs/int.ml";
import "libs/iter.ml";
import "libs/set.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";


let input = io.lines;

let lines = iter.map(str.split, input);
let reports = iter.map_inner((fun x -> x |> int.from_str |> option.unwrap), lines);


let is_safe = fun rep -> begin
  let diffs = vec.collect iter.map(int.sub, iter.sliding_pair rep);
  let minok = iter.all((fun v -> v >= -3), vec.iter diffs);
  let maxok = iter.all((fun v -> v <= 3), vec.iter diffs);
  let incr = iter.all((fun v -> v > 0), vec.iter diffs);
  let decr = iter.all((fun v -> v < 0), vec.iter diffs);  
  minok && maxok && (incr || decr)
end;

let problem_dampener = fun rep -> begin
  let rep = vec.collect rep;
  let is = iter.range(0, vec.length rep);
  let trys = iter.map((fun i -> vec.iter vec.remove_at(rep, i)), is);
  iter.any(is_safe, trys)
end;


let only_safe = iter.filter(problem_dampener, reports);

let result = iter.count(only_safe);


io.write_line ("Day 2, Part 2: " ^ int.to_str result)
