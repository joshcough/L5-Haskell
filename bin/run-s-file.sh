as -o $1.o $1.S
gcc -o $1.out $1.o ../bin/runtime.o
./$1.out
