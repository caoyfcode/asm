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

    add $ZERO_CHAR, %eax # %eax = %eax + '0'
    push %eax
    mov %esp, %eax
    pushl $1
    push %eax
    call printlen # printlen(.., 1)

    mov $0, %eax
    push %eax
    call exit

    .global strlen
strlen: # int print(char *str)
    mov 4(%esp), %ecx # %ecx = str
    xor %eax, %eax # %eax = 0
_strlen_loop:
    mov (%ecx, %eax), %dl # %dl = str[%eax]
    test %dl, %dl # %dl & %dl 的结果影响 elfags, dl = 0 结果为 0, ZF 设位
    jz _strlen_end # % if %dl = 0 goto end
    inc %eax
    jmp _strlen_loop
_strlen_end:
    ret

    .global printlen
printlen: # void printlen(char* str, int32_t len)
    push %ebx # 保存 %ebx
    mov $4, %eax # syscall 4: write
    mov $1, %ebx  # arg0: stdout
    mov 8(%esp), %ecx # arg1: buf
    mov 12(%esp), %edx # arg2: count
    int $0x80     # syscall
    pop %ebx # 恢复 %ebx
    ret

    .global print
print: # void print(char* str)
    mov 4(%esp), %ecx # %ecx = str
    push %ecx
    call strlen
    pop %ecx # %ecx = str
    push %eax # len
    push %ecx # str
    call printlen
    add $8, %esp
    ret

    .global exit
exit: # void exit(int32_t code)
    mov $1, %eax # syscall 1: exit
    mov 4(%esp), %ebx # arg0: err_code
    int $0x80    # syscall
    ret
