#!/bin/bash
if [ main_asm.ml -nt main ]; then
    ocamlopt main.ml -o main 
fi

input=$1
fname="${input%.*}"

cool --type "$fname.cl"
./main "$fname.cl-type"
gcc "$fname.s" --static --no-pie 
