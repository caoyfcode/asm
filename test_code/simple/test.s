.global _start
.section .text
_start:
    mov $1, %eax // 调用号 1, exit
    mov $2, %ebx // 参数 0,  2
    int $0x80