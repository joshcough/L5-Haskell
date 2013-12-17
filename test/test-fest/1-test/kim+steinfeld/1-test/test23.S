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
	movl $11, %eax
	movl $1, %esi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_factorial
L1_Generated_Label_0:
	imull $2, %esi
	addl $1, %esi
	pushl %esi
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_factorial:
	cmpl $2, %eax
	jl L1_base
	jmp L1_recur
L1_base:
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	imull %eax, %esi
	subl $1, %eax
	movl %ebp, %esp
	jmp L1_factorial
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
