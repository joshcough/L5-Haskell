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
	movl $5, %eax
	movl $5, %ebx
	cmpl %ebx, %eax
	je L1_GOOD_PLACE
	jmp L1_good_place
L1_good_place:
	movl $7, %eax
	sall $1, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
L1_GOOD_PLACE:
	movl $13, %eax
	sall $1, %eax
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
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
