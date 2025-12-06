import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";


let parse_line = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.num, 0);
let parse_ops = parsing.seplist(parsing.repeat_min(parsing.ws, 1), parsing.any_char, 0);

let zs = vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parse_line);
let ys = vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parse_line);
let xs = vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parse_line);
let ws = vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parse_line);

let ops = vec.iter option.unwrap parsing.parse(io.read_expected_line {}, parse_ops);


let result = 
  let vars = {mut zs; mut ys; mut xs; mut ws; mut ops; mut total=0} in
    loop match zs {} with
      | `None _ -> `Break vars.total
      | `Some z -> (
        let y = option.unwrap ys {};
        let x = option.unwrap xs {};
        let w = option.unwrap ws {};
        let op = option.unwrap ops {};
        vars.total <- vars.total + (
          if op == "+" then w + x + y + z else
          if op == "*" then w * x * y * z else
          panic op);
        `Continue {}  
      );
    


io.write_line ("Day 6, Part 1: " ^ int.to_str(result));

