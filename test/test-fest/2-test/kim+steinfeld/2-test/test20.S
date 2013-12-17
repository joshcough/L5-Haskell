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
	movl $19, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
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
L1_f:
	addl $-8, %esp
	movl %eax, -8(%ebp)
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_g
L1_Generated_Label_1:
	movl %eax, -4(%ebp)
	movl -8(%ebp), %ebx
	addl %ebx, %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_h
L1_Generated_Label_2:
	movl -4(%ebp), %ebx
	imull $5, %ebx
	addl %ebx, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_g:
	imull %eax, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_h:
	addl %eax, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
