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
	movl $9, %esi
	movl $11, %edi
	pushl %edi
	pushl %esi
	call allocate
	addl $8, %esp
	pushl %eax
	call print
	addl $4, %esp
	movl $13, %esi
	movl $15, %edi
	pushl %edi
	pushl %esi
	call allocate
	addl $8, %esp
	pushl %eax
	call print
	addl $4, %esp
	pushl %esi
	pushl %edi
	call allocate
	addl $8, %esp
	pushl %eax
	call print
	addl $4, %esp
	pushl %esi
	pushl %esi
	call allocate
	addl $8, %esp
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
