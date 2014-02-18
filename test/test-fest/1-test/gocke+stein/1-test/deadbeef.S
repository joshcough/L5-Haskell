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
	pushl $1
	pushl $17
	call allocate
	addl $8, %esp
	movl $27, %rdx
	movl %rdx, 4(%rax)
	movl %rdx, 16(%rax)
	movl $29, %rdx
	movl %rdx, 8(%rax)
	movl %rdx, 24(%rax)
	movl %rdx, 28(%rax)
	movl $21, 12(%rax)
	movl $23, 20(%rax)
	movl $31, 32(%rax)
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
