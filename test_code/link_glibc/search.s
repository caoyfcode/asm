# assembler -o find.o find.s

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
    .global main
main:
    push %ebp
    mov %esp, %ebp

    sub $12, %esp

    # print query msg and number
    movl $query_msg, (%esp)
    call printf
    mov $QUERY, %al
    mov %al, char
    movl $char, (%esp)
    call printf

    # call find
    movl $data, (%esp)
    movl $data_end, 4(%esp)
    movb $QUERY, 8(%esp)
    call find
    mov %eax, 8(%esp)

    # print fing msg and number
    movl $find_msg, (%esp)
    call printf
    mov 8(%esp), %eax
    mov (%eax), %al
    mov %al, char
    movl $char, (%esp)
    call printf
    mov $0, %eax

    mov %ebp, %esp
    pop %ebp
    ret

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
