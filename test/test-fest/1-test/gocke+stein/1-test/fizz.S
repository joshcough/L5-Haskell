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
	movl $0, %rsi
L1_loop:
	movl %rsi, %rcx
	imull $2, %rcx
	addl $1, %rcx
	pushl %rcx
	call print
	addl $4, %esp
	addl $3, %rsi
	cmpl $10, %rsi
	jl L1_loop
	jmp L1_out
L1_out:
	movl %rax, %rax
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
