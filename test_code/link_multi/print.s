// linker -o print print.o lib.o

    .section .data
hello:
    .ascii "Hello, World!"
    .byte 0x0a // LF

    .global _start
    .section .text
_start:
    pushl $14
    pushl $hello
    call printlen
    pushl $0
    call exit