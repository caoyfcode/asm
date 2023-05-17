// assembler -o test_strlen.o test_strlen.s
// linker -o test_strlen test_strlen.o lib.o
// ./test_strlen
// 5

    .set ZERO_CHAR, '0'
    .section .data
hello:
    .string "Hello"

    .section .text
    .global _start
_start:
    mov $hello, %eax
    push %eax
    call strlen

    add $ZERO_CHAR, %eax // %eax = %eax + '0'
    push %eax
    mov %esp, %eax
    pushl $1
    push %eax
    call printlen // printlen(.., 1)

    mov $0, %eax
    push %eax
    call exit