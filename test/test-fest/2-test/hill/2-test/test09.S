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
	movl $13, %rcx
	movl $19, %rdx
	movl $3, %rax
	movl %rdx, %rdi
	movl %rax, %rsi
	pushl $1
	pushl %rcx
	call allocate
	addl $8, %esp
	movl %rax, %rbx
	pushl $1
	pushl %rdi
	call allocate
	addl $8, %esp
	movl $27, 8(%rbx)
	movl %rsi, 8(%rax)
	pushl %rax
	call print
	addl $4, %esp
	pushl %rbx
	call print
	addl $4, %esp
	movl $0, %rax
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
