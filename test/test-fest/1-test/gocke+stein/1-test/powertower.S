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
	movl $2, %eax
	movl $2, %ebx
	movl $2, %ecx
L1_loop1:
L1_loop2:
	imull %eax, %eax
	subl $1, %ebx
	cmpl $0, %ebx
	je L1_finish2
	jmp L1_loop2
L1_finish2:
	movl $2, %ebx
	subl $1, %ecx
	cmpl $0, %ecx
	je L1_finish1
	jmp L1_loop1
L1_finish1:
	imull $2, %eax
	addl $1, %eax
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
