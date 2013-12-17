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
	movl $1, %eax
	movl $1, %ebx
	movl $10, %ecx
L1_loop:
	movl %ebx, %edx
	addl %eax, %ebx
	movl %edx, %eax
	subl $1, %ecx
	cmpl $0, %ecx
	jg L1_loop
	jmp L1_exit
L1_exit:
	sall $1, %ebx
	addl $1, %ebx
	pushl %ebx
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
