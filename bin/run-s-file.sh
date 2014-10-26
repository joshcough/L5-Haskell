#!/bin/sh
as -o $1.o $1.S
gcc -o $1.out $1.o `dirname $0`/runtime.o
./$1.out
