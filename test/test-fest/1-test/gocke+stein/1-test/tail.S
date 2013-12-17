	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %ebx
	pushl %esi
	pushl %edi
	pushl %ebp
	movl	%esp, %ebp
	movl $10000, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_recur_not_recurse
L1_Generated_Label_0:
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_recur_not_recurse:
	cmpl $0, %eax
	je L1_ret
	jmp L1_recur
L1_ret:
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	subl $1, %eax
	movl %ebp, %esp
	jmp L1_recur_not_recurse
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
