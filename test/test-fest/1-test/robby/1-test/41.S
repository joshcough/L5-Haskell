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
	movl $4, %esi
	movl $0, %edi
	movl $1, %ebx
L1_top:
	cmpl %edi, %esi
	je L1_done
	jmp L1_cont
L1_cont:
	pushl %ebx
	pushl $3
	call allocate
	addl $8, %esp
	movl %eax, %ebx
	subl $1, %esi
	jmp L1_top
L1_done:
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
