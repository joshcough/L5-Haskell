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
	movl $27, %rsi
	movl $14, %rdi
	movl %rsi, %rax
	addl %rdi, %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %rsi, %rax
	subl %rdi, %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %rsi, %rax
	imull %rdi, %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %rsi, %rax
	andl %rdi, %rax
	sall $1, %rax
	addl $1, %rax
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
