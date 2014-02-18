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
	movl $9, %rbx
	movl $9, %rsi
	movl $-15, %rdi
	cmp %rsi, %rbx
	setl %al
	movzbl %al, %rax
	cmp %rdi, %rbx
	setl %cl
	movzbl %cl, %rcx
	cmp %rbx, %rdi
	setl %dl
	movzbl %dl, %rdx
	movl %rax, %rbx
	movl %rcx, %rsi
	movl %rdx, %rdi
	sall $1, %rbx
	addl $1, %rbx
	pushl %rbx
	call print
	addl $4, %esp
	sall $1, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	sall $1, %rdi
	addl $1, %rdi
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
