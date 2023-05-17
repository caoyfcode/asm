// assembler -o printf.o printf.s
// gcc -static -m32 -o printf printf.o # 静态链接
// gcc -m32 -no-pie -o printf printf.o # 动态链接, 若不加 -no-pie, 会 warning: relocation in read-only section `.text'
// ./printf

    .section .data
hello:
    .ascii "Hello, World!"
    .byte 0x0a, 0 # LF, \0

    .global main
    .section .text
main:
    push %ebp
    mov %esp, %ebp

    pushl $hello
    call printf
    mov $0, %eax

    mov %ebp, %esp
    pop %ebp
    ret
