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


let xyz_parser = parsing.seplist(parsing.char ",", parsing.num, 3);

let points = 
  io.lines 
  |> (iter.map(fun line -> parsing.parse(line, xyz_parser)))
  |> (iter.map option.unwrap[a=vec@int])
  |> (iter.map(fun xyz -> {x=vec.get(xyz,0); y=vec.get(xyz,1); z=vec.get(xyz,2)}))
  |> vec.collect;

let pids = vec.collect iter.range(0, vec.length points);

let distance = fun (a, b) -> begin
  let a = vec.get(points, a);
  let b = vec.get(points, b);
  let dx = a.x - b.x;
  let dy = a.y - b.y;
  let dz = a.z - b.z;
  dx * dx + dy * dy + dz * dz
end;

let pairwise_distances = fun pids -> begin
  let n = vec.length pids;
  
  (pbar.range(0, n) )
    |> (iter.map(fun i -> (iter.range(0, i) |> (iter.map(fun j -> (i, j))))))
    |> iter.flatten 
    |> (iter.map(fun (pi, pj) -> (vec.get(pids, pi), vec.get(pids, pj))))
    |> (iter.map(fun pair -> {pair; d=distance pair}))
end;


io.write_line "Computing sorted pairwise distances...";
let hp = heap.new (fun ({d=a}, {d=b}) -> a < b);
pids 
  |> pairwise_distances
  |> (iter.for_each(fun p -> 
        heap.push(hp, p)));

let make_queue = fun x ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_back state.data; state.data <- vec.pop_back state.data; x);
    extend = fun it -> state.data <- ((iter.fold(vec.push_back[a=int])) state.data) it
  };

let is_supercomponent = fun graph -> 
  let queue = make_queue option.unwrap (dict.keys graph) {} in
  let vars = {mut visited=set.empty} in
    loop
      match queue.pop {} with
        | `None _ -> `Break (set.length vars.visited == dict.length graph)
        | `Some node -> (if set.contains(vars.visited, node)
              then 
                `Continue {}
              else begin 
                vars.visited <- set.insert(vars.visited, node);
                let out = dict.get_or(graph, node, set.empty);
                queue.extend set.iter out;
                `Continue {}
              end);
  
let unconnected_graph = pids |> vec.iter |> (iter.map (fun p -> (p, set.empty))) |> dict.collect;

let (a, b) = 
  let vars = {mut graph = unconnected_graph; mut n=0; prog=pbar.new heap.length hp} in
  loop begin
    pbar.step(vars.prog, 1);
    vars.n <- vars.n + 1;
    let {pair=(a,b)} = option.unwrap heap.pop hp;
    
    let ea = dict.get_or(vars.graph, a, set.empty);
    let eb = dict.get_or(vars.graph, b, set.empty);

    let ea = set.insert(ea, b);
    let eb = set.insert(eb, a);

    vars.graph <- dict.insert(dict.insert(vars.graph, a, ea), b, eb);

    if vars.n > 1000 && is_supercomponent vars.graph
    then `Break (a, b)
    else `Continue {}
  end;

let a = vec.get(points, a);
let b = vec.get(points, b);

let result = a.x * b.x;

io.write_line ("Day 8, Part 2: " ^ int.to_str(result));

if result <= 64163550 then panic "TOO LOW" else {};
if result >= 9280319378 then panic "TOO HIGH" else {};
