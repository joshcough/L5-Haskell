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
	addl $-16, %esp
	movl %esi, %ebx
	movl %edi, %edi
	movl $3, %ecx
	movl $5, %esi
	movl $7, %eax
	movl $9, %edx
	movl $11, -16(%ebp)
	movl $13, -12(%ebp)
	movl $15, -8(%ebp)
	movl %ecx, -4(%ebp)
	movl -4(%ebp), %ecx
	addl %esi, %ecx
	movl %ecx, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	addl %eax, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	addl %edx, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	movl -16(%ebp), %eax
	addl %eax, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	movl -12(%ebp), %eax
	addl %eax, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	movl -8(%ebp), %eax
	addl %eax, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
	subl $1, %esi
	movl %esi, -4(%ebp)
	movl -4(%ebp), %esi
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
