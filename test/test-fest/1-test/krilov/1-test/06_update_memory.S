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
	pushl $11
	pushl $7
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl $15, 8(%edi)
	movl 4(%edi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 8(%edi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 12(%edi), %eax
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
