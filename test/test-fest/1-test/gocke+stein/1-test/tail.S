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
	movl $10000, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_recur_not_recurse
L1_Generated_Label_0:
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
L1_recur_not_recurse:
	cmpl $0, %rax
	je L1_ret
	jmp L1_recur
L1_ret:
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	subl $1, %rax
	movl %ebp, %esp
	jmp L1_recur_not_recurse
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
