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
	movl $10, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_0:
	movl $7, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_1:
	movl $9, %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_2:
	movl $6, %rax
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_3:
	movl $11, %rax
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_4:
	movl $8, %rax
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_5:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_FACTORIAL:
	movl %rax, %rsi
	movl $0, %rdi
	movl $1, %rax
L1_RERUN:
	addl $1, %rdi
	imull %rdi, %rax
	cmpl %rsi, %rdi
	jl L1_RERUN
	jmp L1_ANSWER
L1_ANSWER:
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $0, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
