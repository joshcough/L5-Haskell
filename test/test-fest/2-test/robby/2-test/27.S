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
	movl %rsi, %rbx
	movl %rdi, %rdi
	pushl $0
	pushl $9
	call allocate
	addl $8, %esp
	movl $3, 4(%rax)
	movl $5, 8(%rax)
	movl $7, 12(%rax)
	movl $9, 16(%rax)
	movl %rax, %rsi
	movl 0(%rsi), %rsi
	sall $1, %rsi
	addl $1, %rsi
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
