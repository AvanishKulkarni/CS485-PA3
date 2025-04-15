#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -g -o main 
fi

input=$1
fname="${input%.*}"
echo "testing: $fname"

rm -f cp1/*.cl-tac
rm -f cp1/*.cl-type
rm -f cp1/*.s

rm -f c3/*.cl-tac
rm -f c3/*.cl-type
rm -f c3/*.s

rm -f *.cl-tac
rm -f *.cl-type
rm -f *.s

cool --type "$fname.cl"
cool --out "$fname"_ref --x86 "$fname".cl
./main "$fname.cl-type"
echo "$fname.s generated"
gcc "$fname.s" --static --no-pie 
echo "a.out generated"

if [ -e "$fname.cl-input" ]; then 
    echo "our output:"
    ./a.out < "$fname.cl-input"
    echo
    echo "cool ref output:"
    cool "$fname.cl" < "$fname.cl-input"
    echo
else 
    echo "our output:"
    ./a.out
    echo
    echo "cool ref output:"
    cool "$fname.cl"
    echo
fi


