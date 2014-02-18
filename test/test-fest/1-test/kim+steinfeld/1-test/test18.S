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
	movl $9, %rsi
	pushl $1
	pushl %rsi
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	addl $4, %rdi
	movl $51, 0(%rdi)
	movl $37, 8(%rdi)
	movl 0(%rdi), %rcx
	movl 8(%rdi), %rdx
	addl %rdx, %rcx
	subl $1, %rcx
	movl %rcx, 0(%rdi)
	movl 0(%rdi), %rdx
	pushl %rdx
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
