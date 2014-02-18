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
	movl $21, %rsi
	pushl $5
	pushl %rsi
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	movl %rdi, %rbx
	addl $20, %rbx
	movl $5, 4(%rdi)
	movl 4(%rdi), %rax
	imull %rax, %rax
	subl $5, %rax
	movl %rax, %rcx
	movl %rax, 16(%rdi)
	imull %rax, %rcx
	movl %rcx, %rdx
	subl $1, %rcx
	movl %rcx, 20(%rdi)
	subl $1, %rdx
	movl %rdx, 16(%rdi)
	movl %rdx, 8(%rdi)
	pushl %rdi
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
