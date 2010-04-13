#!/bin/bash

gcc -O2 -c -o runtime.o runtime.c
as -o test.o test.S
gcc -o a.out test.o runtime.o
