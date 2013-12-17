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
	movl $1000, %eax
	movl $0, %ebx
	movl $1, %ecx
	movl $2, %edx
L1_loop:
	movl %ebx, %edi
	movl %ecx, %ebx
	movl %edx, %ecx
	movl %edi, %edx
	subl $1, %eax
	cmpl $0, %eax
	je L1_finish
	jmp L1_loop
L1_finish:
	imull $2, %ebx
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
