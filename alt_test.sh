#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -o main 
fi

input=$1
fname="${input%.*}"

cool --type "$fname.cl"
./main "$fname.cl-type"
gcc "file.s" --static --no-pie 
./a.out
