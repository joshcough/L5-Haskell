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
	pushl %ebx
	pushl %eax
	call allocate
	addl $8, %esp
	movl %eax, %esi
	pushl $7
	pushl $7
	call allocate
	addl $8, %esp
	movl %eax, 4(%esi)
	pushl $9
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, 8(%esi)
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
