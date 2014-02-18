	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %rbx
	pushl %rsi
	pushl %rdi
	pushl %ebp
	movl	%esp, %ebp
	movl $7, %rax
	sarl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $15, %rax
	sarl $2, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $3, %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $3, %rax
	sall $2, %rax
	addl $3, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $3, %rax
	andl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $27, %rax
	andl $13, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $27, %rax
	movl $13, %rbx
	andl %rbx, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
