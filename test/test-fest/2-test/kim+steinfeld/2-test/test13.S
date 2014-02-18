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
	movl $5, %rbx
	movl %rbx, %rax
	imull %rbx, %rax
	movl $L1_func1, %rbx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp *%rbx
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_func1:
	addl $4, %rax
	movl %ebp, %esp
	jmp L1_func2
L1_func2:
	pushl %rax
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
