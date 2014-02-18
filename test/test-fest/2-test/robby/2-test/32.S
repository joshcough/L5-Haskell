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
	movl $3, %rcx
	movl $5, %rdx
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
	movl %rcx, %rax
	movl %rdx, %rsi
	cmp %rsi, %rax
	setl %al
	movzbl %al, %rax
	addl %rax, %rax
	addl $1, %rax
	cmpl $1, %rax
	je L1_L_3
	jmp L1_L_2
L1_L_3:
	movl $5, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_2:
	movl $3, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
