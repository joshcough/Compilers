#!/bin/bash

gcc -O2 -c -o runtime.o runtime.c
as -o prog.o prog.S
gcc -o a.out prog.o runtime.o
