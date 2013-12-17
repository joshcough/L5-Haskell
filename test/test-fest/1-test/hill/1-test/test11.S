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
L1_LOOP3_SETUP:
	movl $64, %ebx
L1_LOOP3_TOP:
	sarl $1, %ebx
	cmpl $0, %ebx
	je L1_LOOP3_FIN
	jmp L1_LOOP3_TOP
	movl $11, %ecx
	pushl %ecx
	call print
	addl $4, %esp
L1_LOOP3_FIN:
	addl $25, %ebx
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
