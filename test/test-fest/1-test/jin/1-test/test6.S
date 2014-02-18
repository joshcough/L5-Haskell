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
	movl $9, %rax
	subl $4, %esp
	movl %rax, -4(%ebp)
	addl $2, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl -4(%ebp), %rax
	pushl %rax
	call print
	addl $4, %esp
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
