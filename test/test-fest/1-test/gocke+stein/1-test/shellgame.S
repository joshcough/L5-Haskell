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
	movl $1000, %rax
	movl $0, %rbx
	movl $1, %rcx
	movl $2, %rdx
L1_loop:
	movl %rbx, %rdi
	movl %rcx, %rbx
	movl %rdx, %rcx
	movl %rdi, %rdx
	subl $1, %rax
	cmpl $0, %rax
	je L1_finish
	jmp L1_loop
L1_finish:
	imull $2, %rbx
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
