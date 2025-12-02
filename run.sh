#!/bin/bash

# Usage: run.sh <day> <part>

HERE=`pwd`

DAY=$1
PT=$2
YEAR=${3:-2025}
YSRC=$3

cat ~/aoc-data/$YEAR/input-$DAY.txt | ./interpreter -s $HERE/src/${YSRC}/day$DAY-part$PT.ml

