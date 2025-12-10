
// BFS queue
let make_queue = fun (type a) (x: a): {pop: (any -> [`Some a | `None any]); push: (a -> any); extend: ((any -> [`Some a | `None any]) -> any)} ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_front state.data; state.data <- vec.pop_front state.data; x);
    push = fun x -> state.data <- vec.push_back(state.data, x);
  };

// DFS queue
let make_queue = fun (type a) (x: a): {pop: (any -> [`Some a | `None any]); push: (a -> any); extend: ((any -> [`Some a | `None any]) -> any)} ->
  let state = {mut data = #[x]} in {
    pop = fun _ -> (let x = vec.peek_front state.data; state.data <- vec.pop_front state.data; x);
    push = fun x -> state.data <- vec.push_front(state.data, x);
  };

// A* Priority queue
let make_queue = fun x ->
  begin
    let queue = heap.new (fun ({d=d1}, {d=d2}) -> d1 < d2);
    let pop = fun _ -> heap.pop(queue);
    let push = fun x -> heap.push(queue, x);
    heap.push(queue, x);
    {pop; push}
  end;

  NOTE: A* is not quite correct. we need to track and update scores...

// these are the problem-specific functions for the shortest path algorithm
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

let shortest_path = fun problem ->
  let queue = make_queue initial_state problem in
  let vars = {
    mut seen = set.empty
  } in loop begin
    let state = option.unwrap queue.pop {};
    let id = state_id state;
    print state;
    if state.d == 0 then
      `Break state.n
    else if set.contains(vars.seen, id) then 
      `Continue {} 
    else begin
      vars.seen <- set.insert(vars.seen, id);
      following_states(state, problem) |> (iter.for_each queue.push);
      `Continue {}
    end
  end;   