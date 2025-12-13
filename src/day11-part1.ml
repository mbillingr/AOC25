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


let parse_device = 
  parsing.map_result(
    (fun abc -> vec.foldl((fun (l,r)->l^r), "", abc)), 
    parsing.repeat(parsing.any_char, 3, 3));

let parse_devices = parsing.seplist(parsing.ws, parse_device, 1);

let parse_outputs = parsing.sequence(
    parsing.suffixed(parse_device, parsing.text(": ")),
    parse_devices);

let edges = io.lines 
  |> (iter.map(fun line -> parsing.parse(line, parse_outputs)))
  |> (iter.map(fun x -> option.unwrap x))
  |> (iter.map(fun (up, dn) -> (up, set.collect vec.iter dn)))
  |> dict.collect;


let rec count_paths = fun(from) ->
  if from == "out" then
    1
  else
    dict.get(edges, from) 
    |> option.unwrap 
    |> set.iter 
    |> (iter.map count_paths) 
    |> int.sum;

let result = count_paths "you";


io.write_line ("Day 11, Part 1: " ^ int.to_str(result));
