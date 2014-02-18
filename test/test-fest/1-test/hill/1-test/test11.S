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
L1_LOOP3_SETUP:
	movl $64, %rbx
L1_LOOP3_TOP:
	sarl $1, %rbx
	cmpl $0, %rbx
	je L1_LOOP3_FIN
	jmp L1_LOOP3_TOP
	movl $11, %rcx
	pushl %rcx
	call print
	addl $4, %esp
L1_LOOP3_FIN:
	addl $25, %rbx
	pushl %rbx
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
