#!/bin/bash
if [ tac.ml -nt tac ]; then
    ocamlopt tac.ml -g -o tac
fi

if [ "$#" -ne 1 ]; then 
  echo "provide a .cl file argument"
  exit 1
fi

input=$1
fname="${input%.*}"
cool --type "$fname.cl"
./tac "$fname.cl-type"
cool --out "$fname"_ref --tac "$1.cl"
