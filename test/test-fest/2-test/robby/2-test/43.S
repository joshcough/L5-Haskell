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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L_1
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_L_1:
	addl $-16, %esp
	movl %rsi, %rbx
	movl %rdi, %rdi
	movl $3, %rcx
	movl $5, %rsi
	movl $7, %rax
	movl $9, %rdx
	movl $11, -16(%ebp)
	movl $13, -12(%ebp)
	movl $15, -8(%ebp)
	movl %rcx, -4(%ebp)
	movl -4(%ebp), %rcx
	addl %rsi, %rcx
	movl %rcx, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	addl %rax, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	addl %rdx, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	movl -16(%ebp), %rax
	addl %rax, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	movl -12(%ebp), %rax
	addl %rax, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	movl -8(%ebp), %rax
	addl %rax, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	subl $1, %rsi
	movl %rsi, -4(%ebp)
	movl -4(%ebp), %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl %rax, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
