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


let parse_lights = parsing.delimited(
  parsing.char "[",
  parsing.map_result(
    (fun cs -> vec.map((fun c -> if c == "#" then 1 else 0), cs)),
    parsing.repeat_min(parsing.one_of(".#"), 1)), 
  parsing.sequence(
    parsing.char "]",
    parsing.ws));

let parse_buttons = parsing.repeat_min(
  parsing.suffixed(
    parsing.delimited(
      parsing.char "(",
      parsing.seplist(parsing.char(","), parsing.num, 1),
      parsing.char ")"
    ), 
    parsing.ws), 
  0);

let parse_joltages = parsing.delimited(
  parsing.char "{",
  parsing.seplist(parsing.char(","), parsing.num, 1),
  parsing.char "}");

let machine_parser = 
  parsing.sequence(
    parse_lights,
    parsing.sequence(parse_buttons, parse_joltages));

let machine_parser = parsing.map_result(
  (fun (lights, (buttons, joltages)) -> {lights; buttons; joltages}),
  machine_parser);

let machines = io.lines |> (iter.map(fun line -> option.unwrap parsing.parse(line, machine_parser)));

let nonzero = fun x -> x != 0;
let snd_nonzero = fun (_, x) -> x != 0;

let zeros = fun(n_rows, n_cols) ->
  let row = vec.collect iter.repeat(0, n_cols) in
    vec.collect iter.repeat(row, n_rows);

let init_col = 
  let mat_set = fun (mat, i, j, x) -> begin
    let row = vec.get(mat, i);
    let row = vec.set(row, j, x);
    vec.set(mat, i, row)
  end in
  fun(mat, (j, button)) ->
    button 
    |> vec.iter
    |> ((iter.fold (fun(mat, i) -> mat_set(mat, i, j, 1))) mat);

let init_matrix = fun {buttons; joltages} ->
  let mat = zeros(vec.length joltages, vec.length buttons) in
    buttons 
    |> vec.iter
    |> iter.enumerate
    |> ((iter.fold init_col) mat);

let eq_system = fun {buttons; joltages} -> 
  iter.zip(
    joltages |> vec.iter, 
    {buttons; joltages} |> init_matrix |> vec.iter)
  |> (iter.map(fun(jolt, row) -> {b=jolt; row}))
  |> vec.collect;

let col_iter = fun(eqs, j) -> eqs |> vec.iter |> (iter.map (fun {row} -> vec.get(row, j)));

let eq_b_get = fun(eqs, i) -> (vec.get(eqs, i)).b;
let eq_b_set = fun(eqs, i, b) -> begin
  let {row} = vec.get(eqs, i);
  vec.set(eqs, i, {b;row})
end;
let eq_mat_get = fun(eqs, i, j) -> vec.get((vec.get(eqs, i)).row, j);
let eq_mat_set = fun(eqs, i, j, x) -> begin
  let {b; row} = vec.get(eqs, i);
  let row = vec.set(row, j, x);
  vec.set(eqs, i, {b;row})
end;

let eq_swap_rows = vec.swap;

let eq_swap_cols = fun(eqs, i1, i2) -> begin
  vec.iter eqs
  |> (iter.map(fun {b; row} -> {b; row=vec.swap(row, i1, i2)}))
  |> vec.collect
end;

let print_eqs = fun eqs -> begin
  eqs |> vec.iter |> (iter.for_each (fun {b;row} -> (print row, b; 0)));
  io.write_line "=================================================";
  {}
end;

// swap columns so that the diagonal is in the left block if the system is underspecified
let make_left_diag = fun (eqs, limits) -> begin
  let n = (vec.get(eqs, 0)).row |> vec.length;
  let m = vec.length eqs;
  let vars = {mut i=0; mut eqs; mut limits};
  loop begin
    let i = vars.i;
    let eqs = vars.eqs;
    let limits = vars.limits;
    if i == m then
      `Break (eqs, limits)
    else begin
      let x = eq_mat_get(eqs, i, i);
      if x != 0 then
        `Continue (vars.i <- i + 1)
      else begin
        let j = option.unwrap (iter.range(i, n) |> (iter.filter(fun j -> eq_mat_get(eqs, i, j) != 0))) {};
        vars.eqs <- eq_swap_cols(eqs, i, j);
        vars.limits <- vec.swap(limits, i, j);
        `Continue (vars.i <- i + 1)
      end
    end
  end  
end;

// Gaussian elimination
let gauss = fun eq_system ->
  let m = vec.length eq_system in
  let n = vec.length (vec.get(eq_system, 0)).row in
  let vars = {mut h=0; mut k=0; mut eqs = eq_system} in
    loop begin
      let h = vars.h;
      let k = vars.k;
      let eqs = vars.eqs;
      if h >= m || k >= n then
        `Break eqs
      else begin
        let i_max = int.argmin (iter.filter snd_nonzero) (iter.filter (fun (i, _) -> i>=h)) iter.enumerate (iter.map int.abs) col_iter(eqs, k);
        if i_max < 0 then
          vars.k <- k + 1;
          `Continue {}
        else begin          
          vars.eqs <- eq_swap_rows(eqs, h, i_max);
          iter.range(h+1, m) |> (iter.for_each (fun i -> begin
            // todo: lcm or gcm for smaller factors
            let f1 = eq_mat_get(vars.eqs, i, k);
            let f2 = eq_mat_get(vars.eqs, h, k);
            vars.eqs <- eq_mat_set(vars.eqs, i, k, 0);
            let b = eq_b_get(vars.eqs, i) * f2 - eq_b_get(vars.eqs, h) * f1;
            vars.eqs <- eq_b_set(vars.eqs, i, b);
            iter.range(k+1, n) |> (iter.for_each (fun j -> begin 
              let x = eq_mat_get(vars.eqs, i, j) * f2 - eq_mat_get(vars.eqs, h, j) * f1;
              vars.eqs <- eq_mat_set(vars.eqs, i, j, x)
            end))
          end));
          vars.h <- h + 1;
          vars.k <- k + 1;
          `Continue {}
        end
      end
    end;

let rec solution = fun(eq_system, n, vars) -> 
  if n < 0 then `Some vars else begin

  let {b;row} = match vec.peek(eq_system, n) with
    | `Some eq -> eq
    | `None _ -> {b=0; row=#[]};

  let d = iter.zip(vec.iter_rev row, vec.iter_rev vars)
  |> (iter.map int.mul)
  |> int.sum;
  let b_ = b - d;

  match vec.peek(row, n) with
    | `Some x -> (if b_ % x != 0 || b_ / x < 0 then 
          `None {}
        else 
          solution(eq_system, n - 1, vec.push_front(vars, b_ / x)))
    | `None _ -> 
        panic "UNDERDETERMINED"
end;

let bfs_queue = fun (type a) (x: a): {pop: (any -> [`Some a | `None any]); push: (a -> any); extend: ((any -> [`Some a | `None any]) -> any)} ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_front state.data; state.data <- vec.pop_front state.data; x);
    push = fun x -> state.data <- vec.push_back(state.data, x);
    extend = fun it -> state.data <- ((iter.fold(vec.push_back[a=a])) state.data) it
  };

let vec_inc = fun (xs, i) -> vec.set(xs, i, 1 + vec.get(xs, i));
let not = fun f -> fun x -> if f x then false else true;

let dbg = fun x -> (print x; x);
let dbg2 = fun x -> (print x; x);

let incvecs = fun (start, maximum) ->
  let n = vec.length start in
  let seen = set.obj set.empty in
    let queue = bfs_queue start in
      fun _ -> loop begin
        match queue.pop {} with
          | `None _ -> `Break `None {}
          | `Some x -> (
            if seen.contains x then
              `Continue {}
            else if (iter.any (fun (val, max) -> val > max)) (iter.zip(vec.iter_rev x, vec.iter_rev maximum))  then
              `Continue {}
            else begin
              seen.insert x;
              queue.extend (iter.map (fun i -> vec_inc(x, i))) iter.range(0,n);
              `Break `Some x
            end)
      end;

let mingz = fun(a, b) ->
  if a == 0 then b
  else if b == 0 then a
  else int.min(a, b);

let get_limits = fun (eqs) ->
  let row_limits = eqs |> vec.iter |> (iter.map(fun {b;row} -> vec.map((fun x->x*b), row))) in
    let fst = option.unwrap row_limits {} in
      ((iter.fold(fun (a,b)->vec.elementwise(mingz,a,b)) ) fst) row_limits;

let dot = fun(xs: vec@int, ys: vec@int): int ->
  int.sum vec.iter vec.elementwise(int.mul, xs, ys);


let solve = fun (nr, eq_system0) -> begin
  let n = (vec.get(eq_system0, 0)).row |> vec.length;
  
  //print_eqs eq_system0;
  let limits = get_limits eq_system0;
  let eq_system = gauss eq_system0;
  let eq_system = vec.filter((fun {row} -> row |> vec.iter |> (iter.any nonzero)), eq_system);
  let eq_system = vec.map(
    (fun {b; row} -> 
      let d = ((iter.fold int.gcd) b) vec.iter row
      in {b=b/d; row=vec.map((fun x -> x/d), row)}), 
    eq_system);
  //print_eqs eq_system;  
  let (eq_system, limits) = make_left_diag(eq_system, limits);
  //print_eqs eq_system;  
  //print "LIMITS:", limits;

  let m = vec.length eq_system;

  let max_total = eq_system0 |> vec.iter |> (iter.map (fun {b} -> b)) |> int.sum;
  //print nr, m, n, max_total;

  let search_space = incvecs(vec.collect iter.repeat(0, n - m), limits);

  
  let solution = search_space
  |> (iter.map (fun vars -> solution(eq_system, m - 1, vars)))
  |> iter.filter_good
  |> (iter.map vec.iter[a=int])
  |> (iter.map int.sum)
  |> ((iter.fold int.min) max_total);
  //|> (iter.map (fun xs -> (int.sum vec.iter xs, xs)))
  //|> ((iter.fold (fun ((x, xs), (y, ys)) -> if y < x then (y, ys) else (x, xs))) (max_total * 2, #[]));

  solution
end;

let result = 0;

let machines = machines |> vec.collect |> vec.iter_pbar;
let eqsystems = machines |> (iter.map eq_system) |> iter.enumerate;
let eqsystems = eqsystems;
let solutions = eqsystems |> (iter.map solve);
let result = int.sum solutions;

io.write_line ("Day 10, Part 2: " ^ int.to_str(result));

if result <= 14843 then panic "TOO LOW" else {};
