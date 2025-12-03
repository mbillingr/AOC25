import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";

let banks = iter.map(str.chars, io.lines);
let banks = iter.map_inner(int.from_str, banks);
let banks = iter.map_inner(option.unwrap[a=int], banks);
let banks = iter.map(vec.collect[a=int], banks);

let sum = fun (it) -> iter.fold((fun (a,b) -> a + b), 0, it);
let max = fun (it) -> iter.fold((fun (a,b) -> if a > b then a else b), -9999999999, it);
let maxidx = fun (it) -> iter.fold((fun ((i, a),(j, b)) -> if b >= a then (j, b) else (i, a)), (-1, 0), iter.enumerate it);


let find_max_joltage = fun bank -> begin
  let (ridx, ten) = maxidx iter.skip(1, vec.iter_rev bank);
  let idx = vec.length(bank) - ridx - 2;
  print bank;
  print (idx, ten);
  print vec.peek(bank, idx);
  let (_, rhs) = vec.split(bank, idx+1);
  print rhs;
  let one = max vec.iter rhs;
  print one;
  ten * 10 + one
end;


let joltages = iter.map(find_max_joltage, banks);

let total = sum joltages;

io.write_line ("Day 1, Part 1: " ^ int.to_str total);

