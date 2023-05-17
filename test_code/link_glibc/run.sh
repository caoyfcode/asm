#!/bin/bash

for src in `ls *.s | grep -v printf2.s`; do
    echo "-----"  $src "-----"
    filename=$(basename "$src")
    extension="${filename##*.}"
    filename="${filename%.*}"
    /home/caoyf/code/rust/asm/target/debug/assembler -o "$filename".o $src
    gcc -static -m32 -o "$filename" "$filename".o
    ./"$filename"
    echo ""
done