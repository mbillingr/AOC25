import "libs/dict.ml";
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

let inc = fun (pos, cnt) -> (pos + 1, cnt);
let dec = fun (pos, cnt) -> (pos - 1, cnt);

let combine_worlds = fun(cnt1, cnt2) -> cnt1 + cnt2;

let startpos = {} |> io.read_expected_line |> str.chars |> (iter.map not (eq "S")) |> iter.find_idx;

let get_splitpos = fun line -> line |> str.chars |> iter.enumerate |> (iter.filter (fun (i, ch) -> ch == "^")) |> (iter.map (fun (i, ch) -> i));

let result = let vars = {mut timelines = #{startpos:1}} in
  loop match io.lines {} with
    | `None _ -> `Break int.sum dict.vals vars.timelines
    | `Some line -> begin      
      let splits = set.collect get_splitpos line;

      let split_beams = dict.intersection(vars.timelines, splits);
      let intact_beams = dict.difference(vars.timelines, splits);

      let beams_out = intact_beams;
      let beams_out = dict.merge(combine_worlds, beams_out, (iter.map dec) dict.items split_beams);
      let beams_out = dict.merge(combine_worlds, beams_out, (iter.map inc) dict.items split_beams);

      vars.timelines <- beams_out;

      `Continue {}
    end;


io.write_line ("Day 7, Part 2: " ^ int.to_str(result));

