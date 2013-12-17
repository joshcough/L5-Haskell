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
	movl $3, %eax
	addl $4, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $6, %eax
	movl $3, %ebx
	subl %ebx, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $3, %eax
	movl $7, %ebx
	imull %eax, %ebx
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
