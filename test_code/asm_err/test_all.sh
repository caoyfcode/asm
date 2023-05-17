#!/bin/bash

for src in `ls *.s`; do
    echo "-----"  $src "-----"
    /home/caoyf/code/rust/asm/target/debug/assembler -o test.o $src
    echo ""
done