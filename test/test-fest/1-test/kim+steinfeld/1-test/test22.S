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
	movl $7, %ecx
	pushl $5
	pushl %ecx
	call allocate
	addl $8, %esp
	movl $13, 4(%eax)
	movl $19, 8(%eax)
	movl $29, 12(%eax)
	movl $7, %ecx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_squarearray
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
L1_squarearray:
	movl $3, %esi
L1_loop:
	cmpl %ecx, %esi
	jle L1_if
	jmp L1_end
L1_if:
	movl %esi, %edi
	subl $1, %edi
	sarl $1, %edi
	imull $4, %edi
	addl %eax, %edi
	movl 0(%edi), %ebx
	subl $1, %ebx
	sarl $1, %ebx
	imull %ebx, %ebx
	sall $1, %ebx
	addl $1, %ebx
	movl %ebx, 0(%edi)
	addl $2, %esi
	jmp L1_loop
L1_end:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
