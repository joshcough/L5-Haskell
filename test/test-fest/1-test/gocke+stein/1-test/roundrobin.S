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
	movl $72, %rax
	movl %rax, %rbx
	movl %rax, %rcx
	movl %rax, %rdx
	movl %rax, %rsi
	movl %rax, %rdi
	addl $1, %rbx
	movl %rbx, %rax
	movl %rbx, %rcx
	movl %rbx, %rdx
	movl %rbx, %rsi
	movl %rbx, %rdi
	addl $1, %rcx
	movl %rcx, %rax
	movl %rcx, %rbx
	movl %rcx, %rdx
	movl %rcx, %rsi
	movl %rcx, %rdi
	addl $1, %rdx
	movl %rdx, %rax
	movl %rdx, %rbx
	movl %rdx, %rcx
	movl %rdx, %rsi
	movl %rdx, %rdi
	addl $1, %rsi
	movl %rsi, %rax
	movl %rsi, %rbx
	movl %rsi, %rcx
	movl %rsi, %rdx
	movl %rsi, %rdi
	addl $1, %rdi
	movl %rdi, %rax
	movl %rdi, %rbx
	movl %rdi, %rcx
	movl %rdi, %rdx
	movl %rdi, %rsi
	imull $2, %rax
	addl $1, %rax
	imull $2, %rbx
	addl $1, %rbx
	imull $2, %rcx
	addl $1, %rcx
	imull $2, %rdx
	addl $1, %rdx
	imull $2, %rsi
	addl $1, %rsi
	imull $2, %rdi
	addl $1, %rdi
	pushl %rax
	call print
	addl $4, %esp
	pushl %rsi
	call print
	addl $4, %esp
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
