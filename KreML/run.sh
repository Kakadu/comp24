dune exec comp < test.ml
llc -filetype=obj -o out.o out.ll
gcc -c runtime/runtime.c -o runtime/runtime.o
gcc out.o runtime/runtime.o -o out
./out