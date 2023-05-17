    .section .data
a:
    .byte 'a'

    .section .text
    .global _start
_start:
    pushl $1
    mov a, %cl
    pushl %ecx

    call add
    pushl %eax
    mov %esp, %eax

    pushl $1 // len = 1
    pushl %eax // addr of 'b'
    call printlen

    pushl $0
    call exit


add: // int add(int, int)
    mov 4(%esp), %eax
    mov 8(%esp), %ebx
    add %ebx, %eax
    ret

    .section .text
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