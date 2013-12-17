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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L_1
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_L_1:
	movl %esi, %ebx
	movl %edi, %edi
	movl $5, %esi
	sarl $1, %esi
	movl $7, %eax
	sarl $1, %eax
	imull %esi, %eax
	addl %eax, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl %eax, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
