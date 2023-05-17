// assembler -o bss_printf.o bss_printf.s
// gcc -static -m32 -o bss_printf bss_printf.o # 静态链接

    .equ LEN, 6

    .section .bss
hello:
    .zero LEN
    .zero 1

    .global main
    .section .text
main:
    push %ebp
    mov %esp, %ebp

    mov $hello, %eax
    movb $0x48, (%eax)  # 'H'
    movb $0x65, 1(%eax) # 'e'
    movb $0x6c, 2(%eax) # 'l'
    movb $0x6c, 3(%eax) # 'l'
    movb $0x6f, 4(%eax) # 'o'
    movb $0xa, 5(%eax)  # LF
    push %eax
    call printf
    mov $0, %eax

    mov %ebp, %esp
    pop %ebp
    ret
