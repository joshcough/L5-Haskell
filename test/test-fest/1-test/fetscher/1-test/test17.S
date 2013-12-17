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
	pushl $1
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl 0(%esi), %eax
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $5, 4(%esi)
	movl $7, 8(%esi)
	movl 4(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 8(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	pushl %esi
	call print
	addl $4, %esp
	movl $9, 16(%esi)
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
