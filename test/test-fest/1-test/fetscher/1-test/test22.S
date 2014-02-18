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
	pushl %rbx
	pushl %rax
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	pushl $7
	pushl $7
	call allocate
	addl $8, %esp
	movl %rax, 4(%rsi)
	pushl $9
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, 8(%rsi)
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
