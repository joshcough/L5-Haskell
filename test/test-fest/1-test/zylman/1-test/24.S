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
	movl $5, %ecx
	movl $0, %ebx
L1_top:
	subl $1, %ecx
	addl $1, %ebx
	cmpl $0, %ecx
	je L1_end
	jmp L1_top
L1_end:
	addl %ebx, %ebx
	addl $1, %ebx
	pushl %ebx
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
