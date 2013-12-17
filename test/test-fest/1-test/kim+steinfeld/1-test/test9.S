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
	movl $13, %edx
	movl $7, %ecx
	imull $2, %ecx
	subl $1, %ecx
	cmpl %ecx, %edx
	je L1_if
	jmp L1_else
L1_if:
	imull $2, %ecx
	subl $1, %ecx
L1_else:
	pushl %ecx
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
