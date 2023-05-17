// ./test_strlen
// 5

    .set ZERO_CHAR, '0'
    .section .data
hello:
    .string "Hello"

    .section .text
    .global main
main:
    push %ebp
    mov %esp, %ebp

    mov $hello, %eax
    push %eax
    call strlen

    add $ZERO_CHAR, %eax // %eax = %eax + '0'
    push %eax
    call putchar
    mov $0, %eax

    mov %ebp, %esp
    pop %ebp
    ret