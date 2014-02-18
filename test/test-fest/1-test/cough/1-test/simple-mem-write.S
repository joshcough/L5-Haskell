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
	pushl $3
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	movl $5, 4(%rdi)
	movl $7, 8(%rdi)
	movl $9, 12(%rdi)
	movl $11, 16(%rdi)
	movl 4(%rdi), %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl 8(%rdi), %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl 12(%rdi), %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl 16(%rdi), %rsi
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
