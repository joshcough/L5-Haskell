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
L1_LOOP2_SETUP:
	movl $-1, %rsi
	movl $-32, %rdi
	movl $2, %rax
L1_LOOP2_TOP:
	imull %rax, %rsi
	cmpl %rsi, %rdi
	jle L1_LOOP2_TOP
	jmp L1_LOOP2_FIN
	movl $9, %rcx
	pushl %rcx
	call print
	addl $4, %esp
L1_LOOP2_FIN:
	sall $1, %rsi
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
