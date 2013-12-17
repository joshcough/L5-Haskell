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
L1_LOOP2_SETUP:
	movl $-1, %esi
	movl $-32, %edi
	movl $2, %eax
L1_LOOP2_TOP:
	imull %eax, %esi
	cmpl %esi, %edi
	jle L1_LOOP2_TOP
	jmp L1_LOOP2_FIN
	movl $9, %ecx
	pushl %ecx
	call print
	addl $4, %esp
L1_LOOP2_FIN:
	sall $1, %esi
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
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
