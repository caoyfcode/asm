// assembler -o printf2.o printf2.s
// ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 -o printf2 printf2.o -lc -L /usr/lib32 # 动态链接
// gcc -m32 -nostartfiles -no-pie -o printf2 printf2.o # 动态链接
// gcc -m32 -nostartfiles -static -o printf2 printf2.o # 静态链接不成功, 缺少符号 _init, _fini, _DYNAMIC
// 添加 -static-pie 可以解决没有 _DYNAMIC的问题
// ./printf2

    .section .data
hello:
    .ascii "Hello, World!"
    .byte 0x0a, 0 # LF, \0

    .global _start
    .section .text
_start:
    pushl $hello
    call printf
    pushl $0
    call exit
