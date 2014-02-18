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
	movl $11, %rcx
	pushl $5
	pushl %rcx
	call allocate
	addl $8, %esp
	movl $13, 4(%rax)
	movl $19, 8(%rax)
	movl $29, 12(%rax)
	movl $51, 16(%rax)
	movl $79, 20(%rax)
	movl $20, %rcx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_reverse
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
L1_reverse:
	movl %rcx, %rdx
	movl $4, %rsi
	addl %rax, %rsi
	addl %rax, %rdx
L1_loop:
	cmpl %rdx, %rsi
	je L1_else
	jmp L1_if
L1_if:
	movl 0(%rsi), %rdi
	movl 0(%rdx), %rbx
	movl %rbx, 0(%rsi)
	movl %rdi, 0(%rdx)
	addl $4, %rsi
	subl $4, %rdx
	jmp L1_loop
L1_else:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
