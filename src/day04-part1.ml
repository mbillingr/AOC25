import "libs/dict.ml";
import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";


let chs = iter.map(str.chars, io.lines);

//let example = "..@@.@@@@. @@@.@.@.@@ @@@@@.@.@@ @.@@@@..@. @@.@@@@.@@ .@@@@@@@.@ .@.@.@.@@@ @.@@@.@@@@ .@@@@@@@@. @.@.@@@.@.";
//let chs = iter.map(str.chars, str.split(example));

let chsij = iter.enumerate iter.map(iter.enumerate[a=str], chs);
let chsij = iter.map((fun (i, row) -> iter.map((fun (j, ch) -> (#[i, j], ch)), row)), chsij);
let board = dict.collect iter.flatten chsij;

let ni = 1 + iter.fold(int.max, 0, iter.map((fun (pos) -> vec.peek(pos, 0) |> option.unwrap), dict.keys(board)));
let nj = 1 + iter.fold(int.max, 0, iter.map((fun (pos) -> vec.peek(pos, 1) |> option.unwrap), dict.keys(board)));


let neighbors = fun (pos) ->
    let i = vec.peek(pos, 0) |> option.unwrap in
    let j = vec.peek(pos, 1) |> option.unwrap in
    #[#[(i - 1), j], #[(i + 1), j], #[i, (j - 1)], #[i, (j + 1)], 
      #[(i - 1), (j - 1)], #[(i - 1), (j + 1)], #[(i + 1), (j - 1)], #[(i + 1), (j + 1)]];


let range2d = fun (ni, nj) ->
    iter.flatten iter.map((fun i -> iter.map((fun j -> #[i, j]), iter.range(0, nj))), iter.range(0, ni));


let is_roll = fun(pos) -> dict.get(board, pos) == `Some "@";


let is_reachable = fun (pos) ->
    let n = int.sum iter.map((fun (np) -> if is_roll(np) then 1 else 0), vec.iter neighbors pos) in
    n < 4;


let rolls = iter.filter(is_reachable, iter.filter(is_roll, range2d(ni, nj)));
let result = iter.count(rolls);

print is_reachable(#[1,0]);

io.write_line ("Day 4, Part 1: " ^ int.to_str result);

if result >= 12319 then panic "Too High" else {};

