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
	pushl $7
	pushl $41
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl 0(%eax), %eax
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	pushl %esi
	call print
	addl $4, %esp
	movl 0(%esi), %eax
	subl $10, %eax
	movl %eax, 0(%esi)
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
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
