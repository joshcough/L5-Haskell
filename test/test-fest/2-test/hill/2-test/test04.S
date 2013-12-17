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
	pushl $79
	call print
	addl $4, %esp
	pushl $81
	call print
	addl $4, %esp
	pushl $83
	call print
	addl $4, %esp
	pushl $85
	call print
	addl $4, %esp
	pushl $87
	call print
	addl $4, %esp
	pushl $89
	call print
	addl $4, %esp
	pushl $91
	call print
	addl $4, %esp
	pushl $93
	call print
	addl $4, %esp
	pushl $95
	call print
	addl $4, %esp
	pushl $97
	call print
	addl $4, %esp
	pushl $-81
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
