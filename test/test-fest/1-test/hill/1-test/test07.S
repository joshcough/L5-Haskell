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
	movl $25, %esi
	pushl $1
	pushl %esi
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $9, %ecx
	movl $11, %edx
	movl $47, %edi
	movl %ecx, 0(%esi)
	movl %edx, 4(%esi)
	movl %edi, 8(%esi)
	movl 0(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 4(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 8(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	pushl $1
	pushl $9
	call allocate
	addl $8, %esp
	movl $121, 0(%eax)
	movl 0(%eax), %eax
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
