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


let apply = fun (xs: vec@int, idx:int) : (vec@int) ->
    vec.set(xs, idx, vec.get(xs, idx) - 1);

let push = fun (xs: vec@int) : ((vec@int) -> (vec@int)) ->
    fun button ->
      button 
      |> vec.iter
      |> ((iter.fold apply) xs);


// Priority queue
let make_queue = fun x ->
  begin
    let queue = heap.new (fun ({d=d1}, {d=d2}) -> d1 < d2);
    let pop = fun _ -> heap.pop(queue);
    let push = fun x -> heap.push(queue, x);
    heap.push(queue, x);
    {pop; push}
  end;

let state_id = fun {jolts} -> jolts;
let is_valid = fun {jolts} -> jolts |> vec.iter |> (iter.all (fun x -> x >= 0));
let distance_to_target = fun jolts -> jolts |> vec.iter |> int.sum;
let initial_state = fun {joltages} -> {n=0; d=distance_to_target joltages; jolts=joltages};

let following_states = fun ({n; jolts}, {buttons}) ->
  buttons
  |> vec.iter
  |> (iter.map (push jolts))
  |> (iter.map (fun jolts -> {n=n+1; jolts; d=distance_to_target jolts}))
  |> (iter.filter is_valid);

let minimum_presses = fun machine ->
  let queue = make_queue initial_state machine in
  let vars = {
    mut seen = set.empty
  } in loop begin
    let state = option.unwrap queue.pop {};
    print state;
    if state.d == 0 then
      `Break state.n
    else if set.contains(vars.seen, state_id state) then 
      `Continue {} 
    else begin
      vars.seen <- set.insert(vars.seen, state_id state);
      following_states(state, machine) |> (iter.for_each queue.push);
      `Continue {}
    end
  end;

let is_some = fun x -> match x with | `Some _ -> true | _ -> false;
let maybe_min = (iter.fold (fun (o, x) -> match o with | `None _ -> `Some x | `Some y -> `Some int.min(x, y))) `None {};

let is_valid = fun jolts -> jolts |> vec.iter |> (iter.all (fun x -> x >= 0));
let following_states = fun (jolts, buttons) ->
  buttons
  |> vec.iter
  |> (iter.map (push jolts))
  |> (iter.filter is_valid);

let solve = fun machine -> begin
  let vars = {mut memo=dict.empty};
  let rec recursive = fun jolts ->
    if distance_to_target(jolts) == 0 then
      `Some 0
    else
      match dict.get(vars.memo, jolts) with
        | `Some x -> x
        | `None _ ->
      begin
        let res = following_states(jolts, machine.buttons)
          |> (iter.map recursive)
          |> (iter.filter is_some)
          |> (iter.map option.unwrap[a=int])
          |> (iter.map (fun x -> x + 1))
          |> maybe_min;
        vars.memo <- dict.insert(vars.memo, jolts, res);
        print jolts, res;
        res
      end;

  recursive machine.joltages
end;

let result = 0; //machines |> vec.collect |> vec.iter_pbar |> (iter.map minimum_presses) |> int.sum;

//machines {};
print minimum_presses option.unwrap machines {};
//print solve option.unwrap machines {};


io.write_line ("Day 10, Part 1: " ^ int.to_str(result));
