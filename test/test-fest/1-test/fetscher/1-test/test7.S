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
	movl $10, %rax
	sall $1, %rax
	addl $1, %rax
	pushl $0
	pushl %rax
	call allocate
	addl $8, %esp
	movl $1, %rcx
L1_loop:
	movl %rcx, %rdx
	imull $4, %rdx
	addl %rax, %rdx
	movl %rcx, %rbx
	imull $2, %rbx
	addl $1, %rbx
	movl %rbx, 0(%rdx)
	addl $1, %rcx
	cmpl $10, %rcx
	jle L1_loop
	jmp L1_exit
L1_exit:
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
