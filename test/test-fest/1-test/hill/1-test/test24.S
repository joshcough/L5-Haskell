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
	movl $8, %eax
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
	movl %eax, %edi
	movl %eax, %ecx
	sall $1, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl %edi, %eax
	cmpl $0, %eax
	jg L1_GO_A
	jmp L1_QUIT_A
L1_GO_A:
	subl $2, %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_B
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT_A:
	movl %ebp, %esp
	popl %ebp
	ret
L1_FN_B:
	movl %eax, %edi
	movl %eax, %ecx
	sall $1, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	movl %edi, %eax
	cmpl $0, %eax
	jg L1_GO_B
	jmp L1_QUIT_B
L1_GO_B:
	addl $1, %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FN_A
L1_Generated_Label_2:
	movl %ebp, %esp
	popl %ebp
	ret
L1_QUIT_B:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
