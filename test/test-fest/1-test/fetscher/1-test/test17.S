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
	pushl $1
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl 0(%rsi), %rax
	imull $2, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $5, 4(%rsi)
	movl $7, 8(%rsi)
	movl 4(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	movl 8(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	pushl %rsi
	call print
	addl $4, %esp
	movl $9, 16(%rsi)
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
