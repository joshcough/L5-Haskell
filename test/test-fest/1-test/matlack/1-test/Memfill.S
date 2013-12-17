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
	movl $21, %esi
	pushl $5
	pushl %esi
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl %edi, %ebx
	addl $20, %ebx
	movl $1, 4(%edi)
	movl $3, 8(%edi)
	movl $5, 12(%edi)
	movl $7, 16(%edi)
	movl $9, 20(%edi)
	movl $11, 24(%edi)
	movl $13, 28(%edi)
	movl $15, 32(%edi)
	movl $17, 36(%edi)
	movl $19, 40(%edi)
	movl 28(%edi), %ecx
	pushl %edi
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
