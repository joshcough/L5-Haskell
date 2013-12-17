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
	movl $12, %eax
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
	subl $4, %esp
	movl %eax, 0(%esp)
	movl %eax, %ecx
	sall $1, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl 0(%esp), %eax
	cmpl $0, %eax
	jg L1_GO
	jmp L1_QUIT
L1_GO:
	subl $1, %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_1:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
