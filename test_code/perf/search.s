
    .set QUERY, '3'
    .section .data
data:
    .ascii "iu32ygs"
data_end:
query_msg:
    .string "query is "
find_msg:
    .string "find is "
char:
    .byte 0, 0x0a, 0 # {}, LF, \0

    .section .text
    .global _start
_start:
    push %ebp
    mov %esp, %ebp

    sub $12, %esp

    # print query msg and number
    movl $query_msg, (%esp)
    call print
    mov $QUERY, %al
    mov %al, char
    movl $char, (%esp)
    call print

    # call find
    movl $data, (%esp)
    movl $data_end, 4(%esp)
    movb $QUERY, 8(%esp)
    call find
    mov %eax, 8(%esp)

    # print fing msg and number
    movl $find_msg, (%esp)
    call print
    mov 8(%esp), %eax
    mov (%eax), %al
    mov %al, char
    movl $char, (%esp)
    call print

    # exit
    movl $0, (%esp)
    call exit

    .global find
find: # char* find(char* start, char* end, char value), 失败则返回 end*
    push %ebp
    mov %esp, %ebp
    push %ebx # 保存寄存器

    xor %ebx, %ebx # %ebx = 0
    mov 12(%esp), %eax # startn
    mov 16(%esp), %edx # end
    mov 20(%esp), %cl # value
loop:
    cmp %eax, %edx # %edx - %eax
    jbe end # if end <= %eax goto end
    movb (%eax), %bl # %bl = start[index]
    cmp %cl, %bl # %ebx - %ecx
    je end # if (%eax) == value goto end
    inc %eax
    jmp loop
end:
    pop %ebx
    mov %ebp, %esp
    pop %ebp
    ret

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
