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
	movl %rax, %rbx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fac
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
L1_fac:
	cmpl $1, %rbx
	je L1_base
	jmp L1_recure
L1_base:
	imull $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_recure:
	movl %rax, %rdx
	subl $1, %rax
	imull %rdx, %rax
	subl $1, %rbx
	subl $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
