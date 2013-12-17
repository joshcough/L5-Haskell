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
	movl $15, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_FN_A:
	movl %eax, %esi
	movl %eax, %ecx
	sall $1, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl %esi, %eax
	cmpl $0, %eax
	jg L1_GO
	jmp L1_QUIT
L1_GO:
	subl $1, %eax
	movl %ebp, %esp
	jmp L1_FN_A
L1_QUIT:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
