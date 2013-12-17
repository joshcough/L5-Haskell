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
	movl $11, %ecx
	pushl $5
	pushl %ecx
	call allocate
	addl $8, %esp
	movl $13, 4(%eax)
	movl $19, 8(%eax)
	movl $29, 12(%eax)
	movl $51, 16(%eax)
	movl $79, 20(%eax)
	movl $20, %ecx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_reverse
L1_Generated_Label_0:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_reverse:
	movl %ecx, %edx
	movl $4, %esi
	addl %eax, %esi
	addl %eax, %edx
L1_loop:
	cmpl %edx, %esi
	je L1_else
	jmp L1_if
L1_if:
	movl 0(%esi), %edi
	movl 0(%edx), %ebx
	movl %ebx, 0(%esi)
	movl %edi, 0(%edx)
	addl $4, %esi
	subl $4, %edx
	jmp L1_loop
L1_else:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
