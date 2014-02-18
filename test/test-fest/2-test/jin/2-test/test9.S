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
	movl $9, %rsi
	movl $11, %rdi
	pushl %rdi
	pushl %rsi
	call allocate
	addl $8, %esp
	pushl %rax
	call print
	addl $4, %esp
	movl $13, %rsi
	movl $15, %rdi
	pushl %rdi
	pushl %rsi
	call allocate
	addl $8, %esp
	pushl %rax
	call print
	addl $4, %esp
	pushl %rsi
	pushl %rdi
	call allocate
	addl $8, %esp
	pushl %rax
	call print
	addl $4, %esp
	pushl %rsi
	pushl %rsi
	call allocate
	addl $8, %esp
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
