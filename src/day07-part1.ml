import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/curry/iter.ml";
import "libs/set.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";

let eq = fun x -> fun y -> x == y;
let not = fun f -> fun x -> if f x then false else true;
let inc = fun x -> x + 1;
let dec = fun x -> x - 1;

let startpos = {} |> io.read_expected_line |> str.chars |> (iter.map not (eq "S")) |> iter.find_idx;

let get_splitpos = fun line -> line |> str.chars |> iter.enumerate |> (iter.filter (fun (i, ch) -> ch == "^")) |> (iter.map (fun (i, ch) -> i));

let result = let vars = {mut beams = #{startpos:{}}; mut n_splits=0} in
  loop match io.lines {} with
    | `None _ -> `Break vars.n_splits
    | `Some line -> begin      
      let splits = set.collect get_splitpos line;

      let split_beams = set.intersection(vars.beams, splits);
      let intact_beams = set.difference(vars.beams, splits);

      let beams_out = intact_beams;
      let beams_out = set.update(beams_out, (iter.map dec) set.iter split_beams);
      let beams_out = set.update(beams_out, (iter.map inc) set.iter split_beams);

      vars.beams <- beams_out;
      vars.n_splits <- vars.n_splits + set.length split_beams;

      `Continue {}
    end;


io.write_line ("Day 7, Part 1: " ^ int.to_str(result));

