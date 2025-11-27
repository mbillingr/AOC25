import "libs/dict.ml";
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

let left_list = iter.map((fun p -> vec.get(p, 0)), vec.iter parts);
let right_list = iter.map((fun p -> vec.get(p, 1)), vec.iter parts);

let right_counts = dict.count(right_list);

let delta_scores = iter.map((fun x -> x * dict.get_or(right_counts, x, 0)), left_list);

let result = iter.fold(int.add, 0, delta_scores);


print result