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
L1_LOOP1_SETUP:
	movl $1, %eax
	movl $21, %ecx
L1_LOOP1_TOP:
	addl $2, %eax
	cmpl %ecx, %eax
	jl L1_LOOP1_TOP
	jmp L1_LOOP1_FIN
	movl $7, %esi
	pushl %esi
	call print
	addl $4, %esp
L1_LOOP1_FIN:
	pushl %eax
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
