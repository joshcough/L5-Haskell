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
	movl $8, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_FN_A:
	movl %rax, %rdi
	movl %rax, %rcx
	sall $1, %rcx
	addl $1, %rcx
	pushl %rcx
	call print
	addl $4, %esp
	movl %rdi, %rax
	cmpl $0, %rax
	jg L1_GO_A
	jmp L1_QUIT_A
L1_GO_A:
	subl $2, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_B
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT_A:
	movl %ebp, %esp
	popl %ebp
	ret
L1_FN_B:
	movl %rax, %rdi
	movl %rax, %rcx
	sall $1, %rcx
	addl $1, %rcx
	pushl %rcx
	call print
	addl $4, %esp
	movl %rdi, %rax
	cmpl $0, %rax
	jg L1_GO_B
	jmp L1_QUIT_B
L1_GO_B:
	addl $1, %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_2:
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT_B:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
