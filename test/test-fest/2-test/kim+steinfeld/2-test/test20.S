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
	movl $19, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
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
L1_f:
	addl $-8, %esp
	movl %rax, -8(%ebp)
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_g
L1_Generated_Label_1:
	movl %rax, -4(%ebp)
	movl -8(%ebp), %rbx
	addl %rbx, %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_h
L1_Generated_Label_2:
	movl -4(%ebp), %rbx
	imull $5, %rbx
	addl %rbx, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_g:
	imull %rax, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_h:
	addl %rax, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
