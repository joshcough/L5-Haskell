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
	movl $11, %rax
	movl $1, %rsi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_factorial
L1_Generated_Label_0:
	imull $2, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_factorial:
	cmpl $2, %rax
	jl L1_base
	jmp L1_recur
L1_base:
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	imull %rax, %rsi
	subl $1, %rax
	movl %ebp, %esp
	jmp L1_factorial
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
