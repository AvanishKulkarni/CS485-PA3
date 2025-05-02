#!/bin/bash
function run_tests() {
    rm -f "$1".s
    rm -f "$1".cl-type
    rm -f "$1".cl-tac
    rm -f reference_error.txt
    rm -f test_error.txt
    rm -f reference_output.txt
    rm -f test_output.txt
}

rm -f cp1/*.s
rm -f cp1/*.cl-type

if [ -n "$1" ]; then 
    input=$1
    fname="${input%.*}"
    run_tests $fname
else
    for file in cp1/*; do
        if [[ $file == *.cl ]]; then
            fname="${file%.*}"
            run_tests $fname
        fi
    done
    for file in c3/*; do
        if [[ $file == *.cl ]]; then
            fname="${file%.*}"
            run_tests $fname
        fi
    done
fi