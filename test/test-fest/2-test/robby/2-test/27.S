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
	pushl $0
	pushl $9
	call allocate
	addl $8, %esp
	movl $3, 4(%eax)
	movl $5, 8(%eax)
	movl $7, 12(%eax)
	movl $9, 16(%eax)
	movl %eax, %esi
	movl 0(%esi), %esi
	sall $1, %esi
	addl $1, %esi
	pushl %esi
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
