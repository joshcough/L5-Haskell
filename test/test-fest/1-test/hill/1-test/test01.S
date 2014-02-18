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
	movl $29, %rsi
	movl $-71, %rdi
	movl $0, %rax
	addl %rsi, %rax
	addl %rsi, %rax
	addl %rdi, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $0, %rcx
	addl $31, %rcx
	pushl %rcx
	call print
	addl $4, %esp
	movl $0, %rdx
	addl $47, %rdx
	pushl %rdx
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
