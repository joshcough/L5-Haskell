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
	movl %esi, %esi
	movl %edi, %edi
	movl $1, %ecx
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
L1_Generated_Label_1:
	movl %eax, %ebx
	pushl %ebx
	call print
	addl $4, %esp
	movl %eax, %eax
	movl %esi, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_f:
	movl %esi, %esi
	movl %edi, %edi
	movl %ecx, %ebx
	movl %ebx, %eax
	movl %esi, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
