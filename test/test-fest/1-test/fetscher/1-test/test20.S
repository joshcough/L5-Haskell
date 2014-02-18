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
	movl $1, %rax
	movl $1, %rbx
	movl $10, %rcx
L1_loop:
	movl %rbx, %rdx
	addl %rax, %rbx
	movl %rdx, %rax
	subl $1, %rcx
	cmpl $0, %rcx
	jg L1_loop
	jmp L1_exit
L1_exit:
	sall $1, %rbx
	addl $1, %rbx
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
