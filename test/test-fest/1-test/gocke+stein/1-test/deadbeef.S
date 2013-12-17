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
	pushl $1
	pushl $17
	call allocate
	addl $8, %esp
	movl $27, %edx
	movl %edx, 4(%eax)
	movl %edx, 16(%eax)
	movl $29, %edx
	movl %edx, 8(%eax)
	movl %edx, 24(%eax)
	movl %edx, 28(%eax)
	movl $21, 12(%eax)
	movl $23, 20(%eax)
	movl $31, 32(%eax)
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
