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
	movl $15, %rsi
	sarl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $15, %rsi
	sall $1, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $-64, %rsi
	sarl $3, %rsi
	subl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $-64, %rsi
	sall $3, %rsi
	addl $1, %rsi
	pushl %rsi
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
