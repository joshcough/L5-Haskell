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
	pushl $5
	pushl $5
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $10, %edi
	pushl %esi
	call print
	addl $4, %esp
L1_loop:
	movl 0(%esi), %eax
	sall $1, %eax
	addl $1, %eax
	pushl %eax
	pushl %eax
	call allocate
	addl $8, %esp
	movl %eax, %esi
	pushl %esi
	call print
	addl $4, %esp
	subl $1, %edi
	cmpl $0, %edi
	jg L1_loop
	jmp L1_exit
L1_exit:
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
