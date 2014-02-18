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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_go
L1_Generated_Label_0:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_go:
	movl %rax, %rbx
	addl %rbx, %rbx
	movl %rax, %rcx
	addl $2, %rcx
	cmpl %rcx, %rbx
	jl L1_true
	jmp L1_false
L1_true:
	movl $15, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_false:
	movl %rcx, %rax
	imull %rcx, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
