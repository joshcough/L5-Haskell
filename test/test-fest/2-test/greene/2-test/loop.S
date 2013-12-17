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
	movl $65, %eax
	movl $15, %ebx
	jmp L1_loop2
L1_loop1:
	addl $327, %eax
	subl $1, %eax
L1_loop2:
	subl $3, %eax
	addl $1, %eax
	subl $3, %ebx
	addl $1, %ebx
	cmpl $1, %ebx
	je L1_end
	jmp L1_loop2
L1_end:
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
