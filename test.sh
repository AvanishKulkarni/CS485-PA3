#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -o main 
fi
count=0
total=0
function run_tests() {
    rm -f *.cl-tac
    rm -f *.cl-type
    rm -f cp1/*.cl-tac
    rm -f cp1/*.cl-type
    rm -f reference_error.txt
    rm -f test_error.txt
    cool --type "$1"
    ./main "$1-type" > test_error.txt
    cool "$1" --tac --out temp_ref > reference_error.txt

    if [ -f "temp_ref.cl-tac" ]; then
        diff -b -B -w temp_ref.cl-tac "$1-tac" > /dev/null
    else
        diff -b -B -w reference_error.txt test_error.txt > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo -e "\e[32;1mPassed\e[0m $1"
        return 0
    else 
        echo -e "\e[31;1mFailed\e[0m $1"
        return 1
    fi
}

rm -f cp1/*.cl-tac
rm -f cp1/*.cl-type

if [ -n "$1" ]; then 
    run_tests $1
    cool --type "$1"
    echo "Our Output"
    ./main "$1-type"
    echo "Referenced Compiler"
    cool --tac "$1" --out temp_ref
else
    
    for file in cp1/*; do
        if [[ $file == *.cl ]]; then 
            if run_tests $file; then
                count=$((count + 1))
            fi
            total=$((total + 1))
            echo ""
        fi
    done
    echo "Passed $count/$total test cases"
fi