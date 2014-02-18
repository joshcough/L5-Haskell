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
	movl $42, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_square
L1_Generated_Label_0:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_pimult
L1_Generated_Label_1:
	imull $2, %rax
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
L1_square:
	imull %rax, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_pimult:
	imull $3, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
