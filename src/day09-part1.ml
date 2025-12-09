import "libs/dict.ml";
import "libs/heap.ml";
import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/curry/iter.ml";
import "libs/set.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";
import "libs/pbar.ml";


let xy_parser = parsing.seplist(parsing.char ",", parsing.num, 2);

let points = 
  io.lines 
  |> (iter.map(fun line -> parsing.parse(line, xy_parser)))
  |> (iter.map option.unwrap[a=vec@int])
  |> (iter.map(fun xy -> {x=vec.get(xy,0); y=vec.get(xy,1)}))
  |> vec.collect;

//let points = #[{x=7; y=1}, {x=11; y=1}, {x=11; y=7}, {x=9; y=7}, {x=9; y=5}, {x=2; y=5}, {x=2; y=3}, {x=7; y=3}];

let pids = vec.collect iter.range(0, vec.length points);

let distance = fun (a, b) -> begin
  let a = vec.get(points, a);
  let b = vec.get(points, b);
  let dx = int.abs(a.x - b.x) + 1;
  let dy = int.abs(a.y - b.y) + 1;
  dx * dy
end;

let pairwise_distances = fun pids -> begin
  let n = vec.length pids;
  
  (pbar.range(0, n) )
    |> (iter.map(fun i -> (iter.range(0, i) |> (iter.map(fun j -> (i, j))))))
    |> iter.flatten 
    |> (iter.map(fun (pi, pj) -> (vec.get(pids, pi), vec.get(pids, pj))))
    |> (iter.map distance)
end;

let result = ((iter.fold int.max) 0) pairwise_distances pids;

io.write_line ("Day 9, Part 1: " ^ int.to_str(result));


if result <= 4745540856 then panic "TOO LOW" else {};