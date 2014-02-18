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
L1_LOOP1_SETUP:
	movl $1, %rax
	movl $21, %rcx
L1_LOOP1_TOP:
	addl $2, %rax
	cmpl %rcx, %rax
	jl L1_LOOP1_TOP
	jmp L1_LOOP1_FIN
	movl $7, %rsi
	pushl %rsi
	call print
	addl $4, %esp
L1_LOOP1_FIN:
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
