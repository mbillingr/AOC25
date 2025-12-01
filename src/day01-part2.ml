import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";

let steps = iter.map(str.chars, io.lines);
let steps = iter.map(
  (fun chs -> (
    chs() |> option.unwrap, 
    str.join("", chs) |> int.from_str |> option.unwrap)), 
  steps);
let singlesteps = iter.flatten iter.map((fun (d, n) -> iter.repeat(d,n)), steps);
let singlesteps = iter.map((fun d -> if d == "L" then -1 else 1), singlesteps);

let dials = iter.scan((fun(a,b)->(100+a+b)%100), 50, singlesteps);

let n_zeros =  iter.count iter.filter((fun n -> n == 0), dials);

io.write_line ("Day 1, Part 2: " ^ int.to_str n_zeros);

