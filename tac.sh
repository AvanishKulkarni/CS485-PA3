#!/bin/bash

ocamlopt -o main unix.cmxa str.cmxa *.ml -g


if [ "$#" -ne 1 ]; then 
  echo "provide a .cl file argument"
  exit 1
fi

input=$1
fname="${input%.*}"
cool --type "$fname.cl"
./main "$fname.cl-type"
cool --out "$fname"_ref --tac "$1.cl"
