#!/bin/bash

for src in `ls *.s`; do
    echo "-----"  $src "-----"
    ../../target/debug/assembler -o test.o $src
    echo ""
done
