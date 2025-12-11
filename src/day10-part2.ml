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

//let machine_parser = parsing.sequence(parse_lights, parsing.sequence(parse_buttons, parse_joltages)));
let machine_parser = 
  parsing.sequence(
    parse_lights,
    parsing.sequence(parse_buttons, parse_joltages));

let machine_parser = parsing.map_result(
  (fun (lights, (buttons, joltages)) -> {lights; buttons; joltages}),
  machine_parser);

let machines = io.lines |> (iter.map(fun line -> option.unwrap parsing.parse(line, machine_parser)));

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

let eq_swap_rows = fun(eqs, i1, i2) -> begin
  let r1 = vec.get(eqs, i1);
  let r2 = vec.get(eqs, i2);
  vec.set(vec.set(eqs, i1, r2), i2, r1)
end;

let print_eqs = fun eqs -> begin
  eqs |> vec.iter |> (iter.for_each (fun {b;row} -> (print row, b; 0)));
  io.write_line "=================================================";
  {}
end;

let snd_nonzero = fun (_, x) -> x != 0;

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

let rec solutions = fun(eq_system, n, vars) -> 
  if n < 0 then fun _ -> `None {} else begin

  let {b;row} = match vec.peek(eq_system, n) with
    | `Some eq -> eq
    | `None _ -> {b=0; row=#[]};

  let d = iter.zip(vec.iter_rev row, vec.iter_rev vars)
  |> (iter.map int.mul)
  |> int.sum;
  let b_ = b - d;

  print row, vars, n, b_;

  match vec.peek(row, n) with
    | `Some x -> (if b_ % x != 0 || b_ / x < 0 then 
          fun _ -> `None {}
        else 
          solutions(eq_system, n - 1, vec.push_front(vars, b_ / x)))
    | `None _ -> 
        iter.range(0, 10)
        |> (iter.map(fun i -> solutions(eq_system, n - 1, vec.push_front(vars, i)))) 
        |> iter.flatten
end;

let solve = fun eq_system -> begin
  print_eqs eq_system;
  let eq_system = gauss eq_system;
  print_eqs eq_system;  

  let n = (vec.get(eq_system, 0)).row |> vec.length - 1;

  (solutions(eq_system, n, #[])) {}  
end;

let result = 0;

//let machines = machines |> (iter.map (fun {buttons; joltages} -> {buttons=vec.sort_by_key((fun btn -> vec.length btn), buttons); joltages}));

//print machines {};
print solve eq_system option.unwrap machines {};
//print solve option.unwrap machines {};


io.write_line ("Day 10, Part 1: " ^ int.to_str(result));
