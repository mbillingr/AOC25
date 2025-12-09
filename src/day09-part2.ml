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


let xcoords = points |> vec.iter |> (iter.map(fun {x} -> x)) |> set.collect |> set.iter |> vec.collect |> vec.sort_int;
let ycoords = points |> vec.iter |> (iter.map(fun {y} -> y)) |> set.collect |> set.iter |> vec.collect |> vec.sort_int;

let swap = fun (type a b) (x:a, y:b): (b * a) -> (y, x);

let x_to_grid = let map = xcoords |> vec.iter |> iter.enumerate |> (iter.map swap[a=int; b=int]) |> dict.collect in fun (x) -> option.unwrap dict.get(map, x);
let y_to_grid = let map = ycoords |> vec.iter |> iter.enumerate |> (iter.map swap[a=int; b=int]) |> dict.collect in fun (y) -> option.unwrap dict.get(map, y);


let grid_points = vec.map((fun {x; y}-> {i=x_to_grid(x); j=y_to_grid(y)}), points);


let line = fun ({i=i1; j=j1}, {i=i2; j=j2}) -> begin
  let dx = i2 - i1;
  let dy = j2 - j1;

  if j1 == j2 then
    iter.range(i1, i2) |> (iter.map (fun i -> {i; j=j1}))
  else if i1 == i2 then
    iter.range(j1, j2) |> (iter.map (fun j -> {i=i1; j}))
  else
    panic "no diagonal lines"
end;

let outline = 
  vec.push_back(grid_points, vec.front grid_points)  // close the loop
  |> vec.iter 
  |> iter.sliding_pair
  |> (iter.map line)
  |> iter.flatten
  |> set.collect;

let inci = fun {i; j} -> {i=i + 1; j};
let incj = fun {i; j} -> {i; j=j + 1};
let deci = fun {i; j} -> {i=i - 1; j};
let decj = fun {i; j} -> {i; j=j - 1};

let make_queue = fun (type a) (x: a): {pop: (any -> [`Some a | `None any]); push: (a -> any)} ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_back state.data; state.data <- vec.pop_back state.data; x);
    push = fun x -> state.data <- vec.push_back(state.data, x);
    extend = fun it -> state.data <- ((iter.fold(vec.push_back[a=a])) state.data) it
  };

let fill = fun (outline, start) ->
  let queue = make_queue start in
  let vars = {mut visited=set.empty} in
    loop
      match queue.pop {} with
        | `None _ -> `Break `Some vars.visited
        | `Some pos -> (if set.contains(vars.visited, pos)
              then 
                `Continue {}
              else if pos.i < 0 || pos.j < 0 then
                `Break `None "outside"
              else if set.contains(outline, pos) then
                `Continue {}
              else begin 
                vars.visited <- set.insert(vars.visited, pos);                
                queue.push inci pos;
                queue.push incj pos;
                queue.push deci pos;
                queue.push decj pos;
                `Continue {}
              end);

let start_candidates = #[
  (fun p -> inci incj p),
  (fun p -> deci decj p),
  (fun p -> inci decj p),
  (fun p -> deci incj p)
];

let fill_attempts = start_candidates
  |> vec.iter
  |> (iter.map(fun mkstart -> mkstart vec.get(grid_points, 0)))
  |> (iter.map(fun start -> fill(outline, start)))
  |> (iter.filter(fun res -> match res with | `None _ -> false | `Some f -> set.length f > 0));

let filled = option.unwrap option.unwrap fill_attempts {};

let valid_region = set.union(filled, outline);


let pids = vec.collect iter.range(0, vec.length grid_points);

// this is the slow version that checks the whole rectangle
let check_rect = fun ({i=i1; j=j1}, {i=i2; j=j2}) ->
  (iter.range(int.min(i1, i2), int.max(i1, i2) + 1))
    |> (iter.map(fun i -> (iter.range(int.min(j1, j2), int.max(j1, j2) + 1) |> (iter.map(fun j -> {i; j})))))
    |> iter.flatten
    |> (iter.all(fun ij -> set.contains(valid_region, ij)));

// the faster version only checks the rectangle perimeter
let check_rect = fun ({i=i1; j=j1}, {i=i2; j=j2}) -> begin
  let x1 = int.min(i1, i2);
  let x2 = int.max(i1, i2);
  let y1 = int.min(j1, j2);
  let y2 = int.max(j1, j2);

  iter.chain(
    iter.chain(
      iter.range(x1, x2 + 1) |> (iter.map(fun x -> ({i=x; j=y1}))),
      iter.range(x1, x2 + 1) |> (iter.map(fun x -> ({i=x; j=y2})))
    ),
    iter.chain(
      iter.range(y1, y2 + 1) |> (iter.map(fun y -> ({i=x1; j=y}))),
      iter.range(y1, y2 + 1) |> (iter.map(fun y -> ({i=x2; j=y})))
    )
  )
  |> (iter.all(fun ij -> set.contains(valid_region, ij)))
end;

let area = fun (a, b) -> begin
  let a = vec.get(grid_points, a);
  let b = vec.get(grid_points, b);
  let ax = vec.get(xcoords, a.i);
  let ay = vec.get(ycoords, a.j);
  let bx = vec.get(xcoords, b.i);
  let by = vec.get(ycoords, b.j);
  let dx = int.abs(ax - bx) + 1;
  let dy = int.abs(ay - by) + 1;
  if check_rect(a, b) then
    dx * dy
  else
    -1
end;

let pairwise_areas = fun pids -> begin
  let n = vec.length pids;
  
  (pbar.range(0, n) )
    |> (iter.map(fun i -> (iter.range(0, i) |> (iter.map(fun j -> (i, j))))))
    |> iter.flatten 
    |> (iter.map(fun (pi, pj) -> (vec.get(pids, pi), vec.get(pids, pj))))
    |> (iter.map area)
end;

let result = ((iter.fold int.max) 0) pairwise_areas pids;

io.write_line ("Day 9, Part 2: " ^ int.to_str(result));
