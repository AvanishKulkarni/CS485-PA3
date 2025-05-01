#!/bin/bash
if find . -name '*.ml' -newer main | grep -q .; then
    ocamlopt -o main unix.cmxa str.cmxa *.ml -g
fi
count=0
total=0
function run_tests() {
    rm -f cp1/*.s
    rm -f cp1/*.cl-type
    rm -f c3/*.s
    rm -f c3/*.cl-type
    rm -f reference_error.txt
    rm -f test_error.txt
    rm -f reference_output.txt
    rm -f test_output.txt
    
    cool --type "$1.cl"
    cool --out "$1"_ref --x86 "$1.cl"
    
    ./main "$1.cl-type" > test_error.txt
    if [ -f "$1.s" ]; then
        gcc --no-pie --static "$1.s"
        if [ -e "$1.cl-input" ]; then
            cool "$1.cl" < "$1.cl-input" > reference_output.txt
            ./a.out < "$1.cl-input" > test_output.txt
        else
            cool "$1.cl" > reference_output.txt
            ./a.out > test_output.txt
        fi
        diff -b -B -w reference_output.txt test_output.txt > /dev/null
        # diff -b -B -w temp_ref.cl-tac "$1-tac" > /dev/null
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

rm -f cp1/*.s
rm -f cp1/*.cl-type
rm -f c3/*.s
rm -f c3/*.cl-type
if [ -n "$1" ]; then 
    input=$1
    fname="${input%.*}"
    run_tests $fname
    if [ -e "$fname.cl-input" ]; then 
        echo -e "\e[35;1mOur Output:\e[0m"
        ./a.out < "$fname.cl-input"
        echo
        echo -e "\e[35;1mCool Ref Output:\e[0m"
        cool "$fname.cl" < "$fname.cl-input"
        echo
    else 
        echo -e "\e[35;1mOur Output:\e[0m"
        ./a.out
        echo
        echo -e "\e[35;1mCool Ref Output:\e[0m"
        cool "$fname.cl"
        echo
    fi
else
    for file in cp1/*; do
        if [[ $file == *.cl ]]; then
            fname="${file%.*}"
            if run_tests $fname; then
                count=$((count + 1))
            fi
            total=$((total + 1))
            echo ""
        fi
    done
    for file in c3/*; do
        if [[ $file == *.cl ]]; then
            fname="${file%.*}"
            if run_tests $fname; then
                count=$((count + 1))
            fi
            total=$((total + 1))
            echo ""
        fi
    done
    echo "Passed $count/$total test cases"
fi