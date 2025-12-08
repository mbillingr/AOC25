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

let connected_pairs = 
  let hp = heap.new (fun ({d=a}, {d=b}) -> a < b) in begin
    io.write_line "Computing sorted pairwise distances...";
    pids 
      |> pairwise_distances 
      |> (iter.for_each(fun p -> 
            heap.push(hp, p)));

    io.write_line "Getting closest pairs...";
    pbar.range(0, 1000) 
      |> (iter.map (fun _ -> option.unwrap heap.pop hp)) 
      |> vec.collect
  end;

io.write_line "building graph...";
let edges = connected_pairs |> vec.iter_pbar
  |> ((iter.fold(
        fun (edges, {pair=(a,b)}) -> begin
          let ea = dict.get_or(edges, a, set.empty);
          let eb = dict.get_or(edges, b, set.empty);

          let ea = set.insert(ea, b);
          let eb = set.insert(eb, a);

          let edges = dict.insert(edges, a, ea);
          let edges = dict.insert(edges, b, eb);
          edges
        end
      )) dict.empty);


let make_queue = fun x ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_back state.data; state.data <- vec.pop_back state.data; x);
    extend = fun it -> state.data <- ((iter.fold(vec.push_back[a=int])) state.data) it
  };

io.write_line "finding graph components...";
let comp_sizes = 
  let vars={mut es=edges; mut current_component=set.empty; mut comp_sizes=#[]} in
  loop
    match (dict.keys vars.es) {} with
      | `None _ -> `Break vars.comp_sizes
      | `Some k -> (let queue = make_queue k in 
        loop
          match queue.pop {} with 
            | `None _ -> begin
                vars.comp_sizes <- vec.push_back(vars.comp_sizes, set.length vars.current_component);
                vars.current_component <- set.empty;
                `Break `Continue {}
              end
            | `Some k -> begin
                let out = dict.get_or(vars.es, k, set.empty);
                queue.extend set.iter out;
                vars.es <- dict.remove(vars.es, k);
                vars.current_component <- set.insert(vars.current_component, k);
                `Continue {}
              end);

let comp_sizes = comp_sizes |> vec.sort_int |> vec.reverse;

let result = vec.get(comp_sizes, 0) * vec.get(comp_sizes, 1) * vec.get(comp_sizes, 2);

io.write_line ("Day 8, Part 1: " ^ int.to_str(result));

