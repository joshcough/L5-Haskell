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
	movl $21, %rsi
	pushl $5
	pushl %rsi
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	movl %rdi, %rbx
	addl $20, %rbx
	movl $1, 4(%rdi)
	movl $3, 8(%rdi)
	movl $5, 12(%rdi)
	movl $7, 16(%rdi)
	movl $9, 20(%rdi)
	movl $11, 24(%rdi)
	movl $13, 28(%rdi)
	movl $15, 32(%rdi)
	movl $17, 36(%rdi)
	movl $19, 40(%rdi)
	movl $10, %rax
	movl $0, %rcx
L1_loop:
	cmpl $0, %rax
	je L1_done
	jmp L1_keep_going
L1_keep_going:
	movl 12(%rdi), %rdx
	addl %rdx, %rcx
	subl $1, %rax
	jmp L1_loop
L1_done:
	sall $1, %rcx
	addl $1, %rcx
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
