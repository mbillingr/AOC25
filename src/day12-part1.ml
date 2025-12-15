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

import "libs/dancing_links.ml";


let parse_present = 
  parsing.prefixed(
    parsing.seq(#[parsing.num, (parsing.char ":"), parsing.ws]),
    parsing.repeat(parsing.suffixed(parsing.repeat(parsing.any_char, 3, 3), parsing.ws), 3, 3));

let parse_presents = parsing.seplist(parsing.ws, parse_present, 6);

let parse_region = parsing.sequence(
  parsing.sequence(
    parsing.suffixed(parsing.num, parsing.char "x"), 
    parsing.suffixed(parsing.num, parsing.text ": ")),
  parsing.repeat(parsing.suffixed(parsing.num, parsing.ws), 6, 6));
let parse_regions = parsing.repeat_min(parse_region, 1);

let parse_data = parsing.sequence(parse_presents, parse_regions);

let input = str.join(" ", io.lines);

let (shapes, regions) = option.unwrap parsing.parse(input, parse_data);


// turn a shape into a function returning the covered grid coordinates
let compile_shape = fun shape ->
  vec.iter shape
  |> iter.enumerate
  |> (iter.map (fun (i, row) -> 
      vec.iter row
      |> iter.enumerate
      |> (iter.map (fun (j, ch) -> (i, j, ch)))))
  |> iter.flatten
  |> (iter.filter (fun (_, _, ch) -> ch == "#"))
  |> ((iter.fold 
          (fun (f, (i, j)) -> (fun (r, c) -> vec.push_back(f(r,c), (i+r, j+c)))))
        (fun (r, c) -> #[]));


let flip = fun shape -> vec.reverse shape;

let rot = fun shape ->
  iter.range(0, 3) 
  |> (iter.map (fun i -> 
    iter.range(0, 3)
    |> (iter.map (fun j ->
      vec.get(vec.get(shape, 2 - j), i)))
    |> vec.collect)) 
  |> vec.collect;


// all possible variations of rotating and flipping a shape
let shape_variations = fun shape -> begin
  let a = shape;
  let b = rot a;
  let c = rot b;
  let d = rot c;
  let e = flip a;
  let f = flip b;
  let g = flip c;
  let h = flip d;
  #[a, b, c, d, e, f, g, h] |> vec.iter |> set.collect
end;

print vec.front regions;

let shape_instance_name = fun (shape_nr, instance_nr) -> 
  "S" ^ int.to_str(shape_nr) ^ "-" ^ int.to_str(instance_nr);

let region_position_name = fun (row, col) ->
  "R" ^ int.to_str(row) ^ "/" ^ int.to_str(col);

let check_region = fun ((width, height), counts) -> begin
  let h = dancing_links.new_problem{};

  let vars = {mut columns = dict.empty};

  // add one "required" column for each shape instance
  counts |> vec.iter |> iter.enumerate |> (iter.for_each (fun (c, n) ->
    iter.range(0, n) |> (iter.for_each (fun k -> 
      let name = shape_instance_name(c, k) in
      let col = dancing_links.exactly_once_column(h, name) in
        vars.columns <- dict.insert(vars.columns, name, col)))));

  // add one "optional" column for each position in the region
  iter.range(0, height) |> (iter.for_each (fun i ->
    iter.range(0, width) |> (iter.for_each (fun j ->
      let name = region_position_name(i, j) in
      let col = dancing_links.atmost_once_column(h, name) in
        vars.columns<- dict.insert(vars.columns, name, col)))));

  // add one row for each shape placement
  shapes 
  |> vec.iter 
  |> iter.enumerate 
  |> (iter.for_each (fun (c, shp) ->
    shape_variations shp 
    |> set.iter 
    |> (iter.map compile_shape) 
    |> (iter.for_each (fun shp -> 
      iter.range(0, height - 2)
      |> (iter.for_each (fun row -> 
        iter.range(0, width - 2)
        |> (iter.for_each (fun col ->
          iter.range(0, vec.get(counts, c))
          |> (iter.for_each (fun k ->
            let names = vec.push_front(
                vec.map(region_position_name, shp(row, col)),
                shape_instance_name(c, k))
            in let row = vec.map((fun name -> option.unwrap dict.get(vars.columns, name)), names)
            in dancing_links.add_row(h, row)))))))))));
  
  //  let row = #[region_position_name(i, j)]
  //let row = vec.push_back(shape_instance_name(c, k))
  //dancing_links.add_row(h, row);

  dancing_links.print_matrix h;
  print h.rows, "rows";

  0
end;

print check_region(vec.front regions);


let result = 0;


io.write_line ("Day 12, Part 1: " ^ int.to_str(result));
