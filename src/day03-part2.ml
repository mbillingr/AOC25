import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/itertools.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";

let banks = iter.map(str.chars, io.lines);
let banks = iter.map_inner(int.from_str, banks);
let banks = iter.map_inner(option.unwrap[a=int], banks);
let banks = iter.map(vec.collect[a=int], banks);

let max = fun (it) -> iter.fold((fun (a,b) -> if a > b then a else b), -9999999999, it);
let maxidx = fun (it) -> iter.fold((fun ((i, a),(j, b)) -> if b >= a then (j, b) else (i, a)), (-1, 0), iter.enumerate it);


let find_next_joltage_part = fun (i, remaining_bank) -> begin
  let (ridx, part) = maxidx iter.skip(i, vec.iter_rev remaining_bank);
  let idx = vec.length(remaining_bank) - ridx - i - 1;
  let (_, rhs) = vec.split(remaining_bank, idx+1);
  (part, rhs)
end;

let find_max_joltage = fun bank -> begin
  let vars = {mut joltage = 0; mut remaining_bank=bank; mut i = 12 - 1};
  loop begin
    let (part, rhs) = find_next_joltage_part(vars.i, vars.remaining_bank);
    vars.joltage <- vars.joltage * 10 + part;
    vars.remaining_bank <- rhs;
    if (vars.i <- vars.i - 1) == 0
      then `Break vars.joltage
      else `Continue {}
  end
end;


let joltages = iter.map(find_max_joltage, banks);

let total = int.sum joltages;

io.write_line ("Day 1, Part 1: " ^ int.to_str total);

