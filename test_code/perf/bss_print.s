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

printlen: # void printlen(char* str, int32_t len)
    mov $4, %eax # syscall 4: write
    mov $1, %ebx  # arg0: stdout
    mov 4(%esp), %ecx # arg1: buf
    mov 8(%esp), %edx # arg2: count
    int $0x80     # syscall
    ret

exit: # void exit(int32_t code)
    mov $1, %eax # syscall 1: exit
    mov 4(%esp), %ebx # arg0: err_code
    int $0x80    # syscall
    ret
