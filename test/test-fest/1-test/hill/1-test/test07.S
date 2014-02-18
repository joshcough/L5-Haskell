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
	movl $25, %rsi
	pushl $1
	pushl %rsi
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $9, %rcx
	movl $11, %rdx
	movl $47, %rdi
	movl %rcx, 0(%rsi)
	movl %rdx, 4(%rsi)
	movl %rdi, 8(%rsi)
	movl 0(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	movl 4(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	movl 8(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	pushl $1
	pushl $9
	call allocate
	addl $8, %esp
	movl $121, 0(%rax)
	movl 0(%rax), %rax
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
