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
	movl $4, %eax
	movl %eax, %ebx
	movl $13, %ecx
L1_loop:
	cmpl $1, %ecx
	je L1_done
	jmp L1_keep_going
L1_keep_going:
	imull %ebx, %eax
	subl $1, %ecx
	jmp L1_loop
L1_done:
	sall $1, %eax
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
