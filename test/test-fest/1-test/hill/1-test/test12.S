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
	movl $5, %rax
	movl $5, %rbx
	cmpl %rbx, %rax
	je L1_GOOD_PLACE
	jmp L1_good_place
L1_good_place:
	movl $7, %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
L1_GOOD_PLACE:
	movl $13, %rax
	sall $1, %rax
	addl $1, %rax
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
