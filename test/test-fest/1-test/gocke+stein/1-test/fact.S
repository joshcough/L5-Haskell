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
	movl $10, %eax
	movl %eax, %ebx
L1_loop:
	subl $1, %eax
	cmpl $1, %eax
	je L1_finish
	jmp L1_contloop
L1_contloop:
	imull %eax, %ebx
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
