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
	pushl $21
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $1, %rdi
L1_loop:
	pushl %rsi
	call print
	addl $4, %esp
	movl %rdi, %rax
	imull $4, %rax
	addl %rsi, %rax
	movl 0(%rax), %rcx
	addl $4, %rax
	movl 0(%rax), %rdx
	sarl $1, %rdx
	sarl $1, %rcx
	addl %rcx, %rdx
	sall $1, %rdx
	addl $1, %rdx
	movl %rdx, 0(%rax)
	addl $1, %rdi
	cmpl $10, %rdi
	jl L1_loop
	jmp L1_quit
L1_quit:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
