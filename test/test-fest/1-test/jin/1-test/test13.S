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
	movl $8, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	addl %rax, %rax
	addl $1, %rax
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
	subl $8, %esp
	cmpl $2, %rax
	jl L1_base
	jmp L1_recur
L1_base:
	movl $1, %rax
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	movl %rax, -4(%ebp)
	subl $1, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_1:
	movl %rax, -8(%ebp)
	movl -4(%ebp), %rax
	subl $2, %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_2:
	movl -8(%ebp), %rsi
	addl %rsi, %rax
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
