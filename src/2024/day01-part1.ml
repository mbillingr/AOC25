import "libs/io.ml";
import "libs/int.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";


let input = io.lines;

let parts = iter.map(str.split, input);
let parts = iter.map_inner((fun x -> x |> int.from_str |> option.unwrap), parts);
let parts = iter.map(vec.collect[a=int], parts);
let parts = vec.collect parts;

let left_list = vec.sort_int vec.map((fun p -> vec.get(p, 0)), parts);
let right_list = vec.sort_int vec.map((fun p -> vec.get(p, 1)), parts);

let pairs = iter.zip(vec.iter left_list, vec.iter right_list);
let distances = iter.map((fun ab -> ab |> int.sub |> int.abs), pairs);
let result = iter.fold(int.add, 0, distances);

io.write_line ("Day 1, Part 1: " ^ int.to_str result)