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
	pushl $1
	pushl %esi
	call allocate
	addl $8, %esp
	movl %eax, %edi
	addl $4, %edi
	movl $51, 0(%edi)
	movl $37, 8(%edi)
	movl 0(%edi), %ecx
	movl 8(%edi), %edx
	addl %edx, %ecx
	subl $1, %ecx
	movl %ecx, 0(%edi)
	movl 0(%edi), %edx
	pushl %edx
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
