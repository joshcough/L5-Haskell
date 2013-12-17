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
	movl $10, %eax
	sall $1, %eax
	addl $1, %eax
	pushl $0
	pushl %eax
	call allocate
	addl $8, %esp
	movl $1, %ecx
L1_loop:
	movl %ecx, %edx
	imull $4, %edx
	addl %eax, %edx
	movl %ecx, %ebx
	imull $2, %ebx
	addl $1, %ebx
	movl %ebx, 0(%edx)
	addl $1, %ecx
	cmpl $10, %ecx
	jle L1_loop
	jmp L1_exit
L1_exit:
	pushl %eax
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
