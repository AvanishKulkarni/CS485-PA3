An x86-84 optimizing code generator for the [Classroom Object-Oriented Language](https://theory.stanford.edu/~aiken/software/cool/cool.html). 
This takes in type-checked and semantically sound programs in the form of a .cl-type file (generated my the reference compiler, or our own typechecker) and outputs x86-64 assembly. The assembly can be compiled with ``gcc --static --no-pie`` on Ubuntu 22.04 as it does not generated position independent code. 
The compiler performs some basic optimizations like avoiding unused methods and peephole optimizations to reduce stack usage. 
