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


let parse_present = 
  parsing.prefixed(
    parsing.seq(#[parsing.num, (parsing.char ":"), parsing.ws]),
    parsing.repeat(parsing.suffixed(parsing.repeat(parsing.any_char, 3, 3), parsing.ws), 3, 3));

let parse_presents = parsing.seplist(parsing.ws, parse_present, 6);

let parse_region = parsing.sequence(
  parsing.sequence(
    parsing.suffixed(parsing.num, parsing.char "x"), 
    parsing.suffixed(parsing.num, parsing.text ": ")),
  parsing.repeat(parsing.suffixed(parsing.num, parsing.opt(parsing.ws)), 6, 6));
let parse_regions = parsing.repeat_min(parse_region, 1);

let parse_data = parsing.sequence(parse_presents, parse_regions);

let input = str.join(" ", io.lines);

let (shapes, regions) = option.unwrap parsing.parse(input, parse_data);

let shape_sizes = vec.map((fun s -> int.sum (iter.map (fun ch -> (option.unwrap dict.get(#{"#": 1, ".": 0}, ch)))) iter.flatten (iter.map vec.iter[a=str]) vec.iter s), shapes);


// Test if all presents fit in the region in a trivial 3x3 grid arrangement
let obviously_possible = fun ((width, height), counts) ->
  (width / 3) * (height / 3) >= int.sum vec.iter counts;

let area = fun ((width, height), _) -> width * height;

let coverage = fun ((width, height), counts) ->
  int.sum vec.iter vec.elementwise(int.mul, counts, shape_sizes);

let totals = {mut obviously=0; mut maybe=0; mut certainly_not=0; total=vec.length regions};
regions |> vec.iter |> (iter.for_each (fun region -> begin
  let obvpo = if obviously_possible region then 
    totals.obviously <- totals.obviously + 1;
    "obviously" 
  else 
    "maybe    ";

  let covered = coverage region;
  let avail = area region;
  let percentage = (100 * covered) / avail;  

  if covered >= avail then
    totals.certainly_not <- totals.certainly_not + 1;
    {}
  else {};

  io.write_str obvpo;
  io.write_str "    ";
  if percentage < 100 then io.write_str " " else {};
  if percentage < 10 then io.write_str " " else {};
  io.write_str int.to_str percentage;
  io.write_str "% = ";
  io.write_str int.to_str covered;
  io.write_str "/";
  io.write_str int.to_str avail;
  io.write_str "    ";
  print region;
  {}
end));

totals.maybe <- totals.total - totals.obviously - totals.certainly_not;

print totals;

// Apparently, they either fit trivially or clearly not...
let result = totals.obviously;  


io.write_line ("Day 12, Part 1: " ^ int.to_str(result));

if result <= 530 then panic "TOO LOW" else {};
