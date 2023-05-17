    .set LEN, 10
    .set CHAR, 'a'

    .section .data
str:
    .fill LEN, 1, CHAR

    .global _start
    .section .text
_start:
    pushl $LEN
    pushl $str
    call printlen
    pushl $0
    call exit

printlen: // void printlen(char* str, int32_t len)
    mov $4, %eax // syscall 4: write
    mov $1, %ebx  // arg0: stdout
    mov 4(%esp), %ecx // arg1: buf
    mov 8(%esp), %edx // arg2: count
    int $0x80     // syscall
    ret

exit: // void exit(int32_t code)
    mov $1, %eax // syscall 1: exit
    mov 4(%esp), %ebx // arg0: err_code
    int $0x80    // syscall
    ret