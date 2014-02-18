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
	movl $13, %rdx
	movl $7, %rcx
	imull $2, %rcx
	subl $1, %rcx
	cmpl %rcx, %rdx
	je L1_if
	jmp L1_else
L1_if:
	imull $2, %rcx
	subl $1, %rcx
L1_else:
	pushl %rcx
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
