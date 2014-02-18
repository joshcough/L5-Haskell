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
	movl $12, %rax
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
	subl $4, %esp
	movl %rax, 0(%esp)
	movl %rax, %rcx
	sall $1, %rcx
	addl $1, %rcx
	pushl %rcx
	call print
	addl $4, %esp
	movl 0(%esp), %rax
	cmpl $0, %rax
	jg L1_GO
	jmp L1_QUIT
L1_GO:
	subl $1, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_1:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
