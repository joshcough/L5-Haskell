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
	movl $15, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $9, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl $0, %esi
	pushl %eax
	call print
	addl $4, %esp
	movl $2147483647, %edi
	pushl %edi
	call print
	addl $4, %esp
	movl $-1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $-3, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $-2147483647, %eax
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
