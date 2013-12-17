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
	movl $13, %ecx
	movl $19, %edx
	movl $3, %eax
	movl %edx, %edi
	movl %eax, %esi
	pushl $1
	pushl %ecx
	call allocate
	addl $8, %esp
	movl %eax, %ebx
	pushl $1
	pushl %edi
	call allocate
	addl $8, %esp
	movl $27, 8(%ebx)
	movl %esi, 8(%eax)
	pushl %eax
	call print
	addl $4, %esp
	pushl %ebx
	call print
	addl $4, %esp
	movl $0, %eax
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
