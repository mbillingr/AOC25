import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";


let parse_range = parsing.map_result(
  (fun xs -> (vec.peek(xs, 0) |> option.unwrap, vec.peek(xs, 1) |> option.unwrap)), 
  parsing.seplist(parsing.char "-", parsing.num, 2));

let ranges = iter.map((fun line -> parsing.parse(line, parse_range)), io.lines);
let ranges = iter.map(option.unwrap[a=(int*int)], iter.take_while(option.is_positive, ranges));

//let ranges = vec.iter #[(3,5), (10,14), (16,20), (12,18)];

let range_edges = iter.fold(
  (fun (xs, (a, b)) -> 
    vec.push_back(
      vec.push_back(
        xs, `From a), `To (b + 1))), 
  #[], 
  ranges);

let edge_pos = fun e -> match e with
    | `From a -> a
    | `To a -> a;

let cmp_edges = fun (e1, e2) -> begin
  edge_pos(e1) < edge_pos(e2)
end;

let sorted_edges = vec.sort_by(cmp_edges, range_edges);

let result =
  let vars = {mut start=0; mut overlap=0; mut total=0} in
  let es = vec.iter sorted_edges in
    loop
      match es {} with
        | `None _ -> `Break vars.total
        | `Some e -> (match e with
            | `From x -> (if (vars.overlap <- vars.overlap + 1) == 0
                then `Continue (vars.start <- x)
                else `Continue {})
            | `To x -> (if (vars.overlap <- vars.overlap - 1) == 1
                then `Continue (vars.total <- vars.total + (x - vars.start))
                else `Continue {}));

io.write_line ("Day 5, Part 2: " ^ int.to_str(result));

if result <= 339668510830666 then panic "Too Low" else {};
if result <= 339668510830673 then panic "Too Low" else {};
if result >= 339668510830765 then panic "Too High" else {};

