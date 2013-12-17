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
	movl $5, 4(%edi)
	movl 4(%edi), %eax
	imull %eax, %eax
	subl $5, %eax
	movl %eax, %ecx
	movl %eax, 16(%edi)
	imull %eax, %ecx
	movl %ecx, %edx
	subl $1, %ecx
	movl %ecx, 20(%edi)
	subl $1, %edx
	movl %edx, 16(%edi)
	movl %edx, 8(%edi)
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
