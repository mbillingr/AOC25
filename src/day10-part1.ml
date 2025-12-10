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


let toggle = fun (lights: vec@int, idx:int) : (vec@int) ->
    vec.set(lights, idx, 1 - vec.get(lights, idx));  

let push = fun (lights: vec@int) : ((vec@int) -> (vec@int)) ->
    fun button ->
      button 
      |> vec.iter
      |> ((iter.fold toggle) lights);


let debug = fun (type a) (x:a):a -> (print x; x);


let make_queue = fun (type a) (x: a): {pop: (any -> [`Some a | `None any]); push: (a -> any); extend: ((any -> [`Some a | `None any]) -> any)} ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_front state.data; state.data <- vec.pop_front state.data; x);
    push = fun x -> state.data <- vec.push_back(state.data, x);
    extend = fun it -> state.data <- ((iter.fold(vec.push_back[a=a])) state.data) it
  };


let initial_buttons = fun lights -> vec.map((fun _ -> 0), lights);
let initial_state = fun {lights} -> {n=0; lights=initial_buttons lights};
let state_id = fun {lights} -> lights;
let is_target = fun ({lights=state}, {lights=target}) -> state == target;

let following_states = fun ({n; lights}, {buttons}) ->
  buttons
  |> vec.iter
  |> (iter.map (push lights))
  |> (iter.map (fun lights -> {n=n+1; lights}));

let minimum_presses = fun machine ->
  let queue = make_queue initial_state machine in
  let vars = {
    mut seen = set.empty
  } in loop begin
    let state = option.unwrap queue.pop {};
    if is_target(state, machine) then
      `Break state.n
    else if set.contains(vars.seen, state_id state) then 
      `Continue {} 
    else begin
      vars.seen <- set.insert(vars.seen, state_id state);
      queue.extend following_states(state, machine);
      `Continue {}
    end
  end;   
      

let result = machines |> vec.collect |> vec.iter_pbar |> (iter.map minimum_presses) |> int.sum;


io.write_line ("Day 10, Part 1: " ^ int.to_str(result));
