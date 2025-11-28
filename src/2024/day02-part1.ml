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

let diffs = iter.map_inner(int.sub, iter.map(iter.sliding_pair[a=int], reports));


let is_safe = 
  let aggregate = fun({signs; min; max}, d) -> 
    {
      signs = set.insert(signs, int.sign d);
      min = int.min(min, int.abs(d));
      max = int.max(max, int.abs(d))
    }
  in fun ds ->  
    let {signs; min; max} = iter.fold(aggregate, {signs=set.empty; min=9999; max=-9999}, ds) in
      set.length(signs) == 1 && min >= 1 && max <= 3;


let only_safe = iter.filter(is_safe, diffs);

let result = iter.count(only_safe);


io.write_line ("Day 2, Part 1: " ^ int.to_str result)