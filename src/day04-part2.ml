import "libs/dict.ml";
import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/pbar.ml";
import "libs/vec.ml";


let chs = iter.map(str.chars, io.lines);

//let example = "..@@.@@@@. @@@.@.@.@@ @@@@@.@.@@ @.@@@@..@. @@.@@@@.@@ .@@@@@@@.@ .@.@.@.@@@ @.@@@.@@@@ .@@@@@@@@. @.@.@@@.@.";
//let chs = iter.map(str.chars, str.split(example));

let chsij = iter.enumerate iter.map(iter.enumerate[a=str], chs);
let chsij = iter.map((fun (i, row) -> iter.map((fun (j, ch) -> (#[i, j], ch)), row)), chsij);
let board = dict.collect iter.filter((fun (_, ch) -> ch == "@"), iter.flatten chsij);


let neighbors = fun (pos) ->
    let i = vec.peek(pos, 0) |> option.unwrap in
    let j = vec.peek(pos, 1) |> option.unwrap in
    #[#[(i - 1), j], #[(i + 1), j], #[i, (j - 1)], #[i, (j + 1)], 
      #[(i - 1), (j - 1)], #[(i - 1), (j + 1)], #[(i + 1), (j - 1)], #[(i + 1), (j + 1)]];


let is_roll = fun(board, pos) -> dict.get(board, pos) == `Some "@";


let is_reachable = fun board -> fun pos ->
    let n = int.sum iter.map((fun (np) -> if is_roll(board, np) then 1 else 0), vec.iter neighbors pos) in
    n < 4;


let starting_number_of_rolls = dict.length board;
let progress = pbar.new(starting_number_of_rolls);

let final_board = 
    let vars = {mut board = board} in
    loop begin
        let ps = vec.collect iter.filter(is_reachable vars.board, dict.keys vars.board);
        pbar.step(progress, vec.length ps);
        if vec.length ps == 0 
            then `Break vars.board 
            else `Continue (vars.board <- iter.fold(dict.remove[k=vec@int; v=str], vars.board, vec.iter ps))
    end;

let result = dict.length board - dict.length final_board;

io.write_line ("Day 4, Part 2: " ^ int.to_str result);

if result >= 12319 then panic "Too High" else {};

