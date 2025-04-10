#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -g -o main 
fi

input=$1
fname="${input%.*}"

cool --type "$fname.cl"
./main "$fname.cl-type"
gcc "$fname.s" --static --no-pie -g
gdb ./a.out
