    .equ LEN, 6

    .section .bss
hello:
    .zero LEN

    .global _start
    .section .text
_start:
    mov $hello, %eax
    movb $0x48, (%eax)  # 'H'
    movb $0x65, 1(%eax) # 'e'
    movb $0x6c, 2(%eax) # 'l'
    movb $0x6c, 3(%eax) # 'l'
    movb $0x6f, 4(%eax) # 'o'
    movb $0xa, 5(%eax)  # LF
    pushl $LEN
    push %eax
    call printlen
    pushl $0
    call exit