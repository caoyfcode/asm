    .section .text
    .global strlen
strlen: // int print(char *str)
    mov 4(%esp), %ecx // %ecx = str
    xor %eax, %eax // %eax = 0
loop:
    mov (%ecx, %eax), %dl // %dl = str[%eax]
    test %dl, %dl // %dl & %dl 的结果影响 elfags, dl = 0 结果为 0, ZF 设位
    jz end // % if %dl = 0 goto end
    inc %eax
    jmp loop
end:
    ret

    .global printlen
printlen: // void printlen(char* str, int32_t len)
    push %ebx // 保存 %ebx
    mov $4, %eax // syscall 4: write
    mov $1, %ebx  // arg0: stdout
    mov 8(%esp), %ecx // arg1: buf
    mov 12(%esp), %edx // arg2: count
    int $0x80     // syscall
    pop %ebx // 恢复 %ebx
    ret

    .global print
print: // void print(char* str)
    mov 4(%esp), %ecx // %ecx = str
    push %ecx
    call strlen
    pop %ecx // %ecx = str
    push %eax // len
    push %ecx // str
    call printlen
    add $8, %esp
    ret

    .global exit
exit: // void exit(int32_t code)
    mov $1, %eax // syscall 1: exit
    mov 4(%esp), %ebx // arg0: err_code
    int $0x80    // syscall
    ret

// System V ABI
// This is a 32-bit platform. The stack grows downwards. Parameters to functions are passed on the stack in reverse order such that the first parameter is the last value pushed to the stack, which will then be the lowest value on the stack. Parameters passed on the stack may be modified by the called function. Functions are called using the call instruction that pushes the address of the next instruction to the stack and jumps to the operand. Functions return to the caller using the ret instruction that pops a value from the stack and jump to it. The stack is 4-byte aligned all the time, on older systems and those honouring the SYSV psABI. On some newer systems, the stack is additionally 16-byte aligned just before the call instruction is called (usually those that want to support SSE instructions); consult your manual (GNU/Linux on i386 has recently become such a system, but code mixing with 4-byte stack alignment-assuming code is possible).
// Functions preserve the registers ebx, esi, edi, ebp, and esp; while eax, ecx, edx are scratch registers. The return value is stored in the eax register, or if it is a 64-bit value, then the higher 32-bits go in edx. Functions push ebp such that the caller-return-eip is 4 bytes above it, and set ebp to the address of the saved ebp. This allows iterating through the existing stack frames. This can be eliminated by specifying the -fomit-frame-pointer GCC option.
