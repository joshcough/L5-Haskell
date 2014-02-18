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
	movl $10, %rcx
	movl $3, %rsi
	movl $23, %rax
	movl $15, %rbx
	cmpl %rbx, %rax
	je L1_first
	jmp L1_second
L1_first:
	movl %rax, %rbx
	movl %rbx, %rcx
	jmp L1_end
L1_second:
	addl %rbx, %rbx
L1_end:
	addl %rbx, %rax
	imull %rax, %rcx
	pushl %rax
	pushl %rsi
	call allocate
	addl $8, %esp
	movl 4(%rax), %rdx
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
