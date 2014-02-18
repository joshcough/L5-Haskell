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
	movl %rsi, %rsi
	movl %rdi, %rdi
	movl $1, %rcx
	movl $3, %rdx
	movl $1, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
L1_Generated_Label_1:
	movl %rax, %rbx
	pushl %rbx
	call print
	addl $4, %esp
	movl %rax, %rax
	movl %rsi, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_f:
	movl %rsi, %rbx
	movl %rdi, %rdi
	movl %rcx, %rsi
	movl %rdx, %rdx
	movl %rax, %rax
	cmpl $1, %rsi
	je L1_L_3
	jmp L1_L_2
L1_L_3:
	cmpl $1, %rdx
	je L1_L_5
	jmp L1_L_4
L1_L_5:
	cmpl $1, %rax
	je L1_L_7
	jmp L1_L_6
L1_L_7:
	movl $15, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_6:
	movl $13, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_4:
	cmpl $1, %rax
	je L1_L_9
	jmp L1_L_8
L1_L_9:
	movl $11, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_8:
	movl $9, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_2:
	cmpl $1, %rdx
	je L1_L_11
	jmp L1_L_10
L1_L_11:
	cmpl $1, %rax
	je L1_L_13
	jmp L1_L_12
L1_L_13:
	movl $7, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_12:
	movl $5, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_10:
	cmpl $1, %rax
	je L1_L_15
	jmp L1_L_14
L1_L_15:
	movl $3, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_14:
	movl $1, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
