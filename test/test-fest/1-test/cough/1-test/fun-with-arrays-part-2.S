	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %ebx
	pushl %esi
	pushl %edi
	pushl %ebp
	movl	%esp, %ebp
	pushl $3
	pushl $101
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl $50, %esi
	movl $1, %edx
L1_filler:
	movl %esi, %ecx
	imull $4, %ecx
	addl %edi, %ecx
	movl %edx, %ebx
	imull $2, %ebx
	addl $1, %ebx
	movl %ebx, 0(%ecx)
	subl $1, %esi
	addl $1, %edx
	cmpl $1, %esi
	jl L1_printer
	jmp L1_filler
L1_printer:
	movl $1, %esi
L1_printer2:
	movl %esi, %ecx
	imull $4, %ecx
	addl %edi, %ecx
	movl 0(%ecx), %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl $51, %edx
	addl $1, %esi
	cmpl %edx, %esi
	jl L1_printer2
	jmp L1_ender
L1_ender:
	pushl %edi
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
