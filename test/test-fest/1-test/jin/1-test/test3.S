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
	movl $1, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	subl $2, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_fib:
	cmpl $2, %rax
	jl L1_base
	jmp L1_recur
L1_base:
	movl $31, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	movl $51, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
