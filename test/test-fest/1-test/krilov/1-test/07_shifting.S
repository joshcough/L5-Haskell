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
	movl $3, %eax
	sall $1, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $3, %eax
	sall $3, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $6, %eax
	sarl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $20, %eax
	sarl $2, %eax
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
