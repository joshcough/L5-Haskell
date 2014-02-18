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
	movl $7, %rcx
	pushl $5
	pushl %rcx
	call allocate
	addl $8, %esp
	movl $13, 4(%rax)
	movl $19, 8(%rax)
	movl $29, 12(%rax)
	movl $7, %rcx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_squarearray
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
L1_squarearray:
	movl $3, %rsi
L1_loop:
	cmpl %rcx, %rsi
	jle L1_if
	jmp L1_end
L1_if:
	movl %rsi, %rdi
	subl $1, %rdi
	sarl $1, %rdi
	imull $4, %rdi
	addl %rax, %rdi
	movl 0(%rdi), %rbx
	subl $1, %rbx
	sarl $1, %rbx
	imull %rbx, %rbx
	sall $1, %rbx
	addl $1, %rbx
	movl %rbx, 0(%rdi)
	addl $2, %rsi
	jmp L1_loop
L1_end:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
