#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -g -o main 
fi

input=$1
fname="${input%.*}"
echo "testing: $fname"

cool --type "$fname.cl"
./main "$fname.cl-type"
echo "$fname.s generated"
gcc "$fname.s" --static --no-pie 
echo "a.out generated"

echo "our output:"
./a.out < "$fname.cl-input"
echo

echo "cool ref output:"
cool "$fname.cl" < "$fname.cl-input"
echo
