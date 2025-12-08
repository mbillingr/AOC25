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

//let points = vec.sort_by((fun ({x=a}, {x=b}) -> a < b), points);


let distance = fun (a, b) -> begin
  let a = vec.get(points, a);
  let b = vec.get(points, b);
  let dx = a.x - b.x;
  let dy = a.y - b.y;
  let dz = a.z - b.z;
  dx * dx + dy * dy + dz * dz
end;


let myheap = fun _ -> heap.new (fun (pair1, pair2) -> distance pair1 > distance pair2);

let merge = fun (hp, ps, n) -> begin
  ps |> (iter.for_each(fun x -> heap.push(hp, x)));
  loop
    if heap.length hp > n
    then `Continue (heap.pop hp)
    else `Break hp
end;


let cross_split_closest = fun(l, r, best, n) -> begin
  let (l_, r_) = match heap.peek best with
    | `None _ -> (vec.iter l, vec.iter r)
    | `Some pair -> begin
      let d = distance pair;
      let xrmin = (vec.front r).x;
      let xlmax = (vec.back l).x;
      (
        l |> vec.iter_rev |> (iter.take_while(fun p -> xrmin - p.x <= d)), 
        r |> vec.iter     |> (iter.take_while(fun p -> p.x - xlmax <= d))
      )
    end;
  let r_ = vec.collect r_;
  let pairs = l_ |> (iter.map(fun pl -> (r_ |> vec.iter |> (iter.map(fun pr -> (pl, pr)))))) |> iter.flatten;
  merge(best, pairs, n)
end;


let rec closest = fun(ps, n) ->
  if vec.length ps < 2 then
    myheap {}
  else begin
    let (l, r) = vec.split(ps, vec.length ps / 2);
    let best_l = closest(l, n);
    let best_r = closest(r, n);
    let best = merge(best_l, best_r.data |> vec.iter |> (iter.map(fun x -> x.x)), n);
    print best.data |> vec.iter |> (iter.map(fun x -> x.x)) |> (iter.map distance) |> vec.collect;
    cross_split_closest(l, r, best, n)
  end;

//print closest(pids, 1000);

io.write_line "Computing pairwise distances...";
let n = vec.length points;
let pairs = 
  pbar.range(0, n) 
  |> (iter.map(fun i -> (iter.range(0, i) |> (iter.map(fun j -> (i, j))))))
  |> iter.flatten 
  |> (iter.map(fun pair -> {pair; d=distance pair}))
  |> vec.collect;
io.write_line "sorting pairs by distance...";
let pairs = vec.sort_by((fun ({d=a}, {d=b}) -> a < b), pairs);
let (connected_pairs, _) = vec.split(pairs, 1000);

io.write_line "building graph...";
let nodes_a = connected_pairs |> vec.iter |> (iter.map(fun {pair=(x, _)} -> x)) |> set.collect;
let nodes_b = connected_pairs |> vec.iter |> (iter.map(fun {pair=(_, x)} -> x)) |> set.collect;
let nodes = set.union(nodes_a, nodes_b);

io.write_line "building graph...";
let edges = connected_pairs |> vec.iter
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

