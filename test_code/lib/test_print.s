    .section .data
hello:
    .string "Hello, World!"

    .section .text
    .global _start
_start:
    mov $hello, %eax
    push %eax
    call print
    mov $0, %eax
    push %eax
    call exit
