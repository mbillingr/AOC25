# AOC25
[Advent of Code 2025](https://adventofcode.com/2025)

This year I wrote my solutions in a customized version of polysubml. You can find both, [my modified version](https://github.com/mbillingr/polysubml-demo/tree/aoc-25) and the [original polysubml](https://github.com/Storyyeller/polysubml-demo) here on GitHub.

The puzzle solutions in the repository should work "as is". An interpreter binary and a copy of my "standard" libraries are included. The binary works on my Arch Linux machines, but since it's statically linked there is a good chance it could run on any x86-64 Linux.

## Usage

The solution scripts expect the input to be piped in from stdin. There is a shell script to simplify the process.
For example, to solve day 6, part 2:
```bash
./run 06 2
```

This is equivalent to
```bash
cat ~/aoc-data/2025/input-06.txt | ./interpreter -s src/day06-part2.ml
```
