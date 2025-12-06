import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";


let parse_line = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.num, 0);
let parse_ops = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.any_char, 0);

let ws = str.chars io.read_expected_line {};
let xs = str.chars io.read_expected_line {};
let ys = str.chars io.read_expected_line {};
let zs = str.chars io.read_expected_line {};
let ops = str.chars io.read_expected_line {};
let digits = iter.zip4(ws, xs, ys, zs);

let get_num = fun ns -> begin
  let digits = #[];
  let digits = match int.from_str ns._0 with | `Some k -> vec.push_back(digits, k) | `None _ -> digits;
  let digits = match int.from_str ns._1 with | `Some k -> vec.push_back(digits, k) | `None _ -> digits;
  let digits = match int.from_str ns._2 with | `Some k -> vec.push_back(digits, k) | `None _ -> digits;
  let digits = match int.from_str ns._3 with | `Some k -> vec.push_back(digits, k) | `None _ -> digits;

  if vec.is_empty digits
    then `None {}
    else `Some iter.fold((fun (n, k) -> n*10 + k), 0, vec.iter digits)
  
end;

let make_op = fun op ->
  if op == "+" then let state={mut acc=0} in {get=fun _ -> state.acc; apply=fun x -> state.acc <- state.acc + x} else
  if op == "*" then let state={mut acc=1} in {get=fun _ -> state.acc; apply=fun x -> state.acc <- state.acc * x} else
  {get=fun _ -> panic "invalid"; apply=fun _ -> panic "invalid"};

let nums = iter.map(get_num, digits);

let result = 0;
let result = 
  let current = {mut op=make_op(""); mut total=0} in
    loop begin
      match ops {} with
        | `None _ -> {}
        | `Some op -> (if op == " " then {} else current.op <- make_op op);

      match nums {} with
        | `None _ -> `Break (current.total + current.op.get {})
        | `Some n -> (match n with
          | `None _ -> `Continue (current.total <- current.total + current.op.get {})
          | `Some n -> `Continue (current.op.apply n))
    end;


io.write_line ("Day 6, Part 1: " ^ int.to_str(result));

if result <= 5202707 then panic "Too Low" else {};
