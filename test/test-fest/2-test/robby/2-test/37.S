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
	movl $3, %edx
	movl $3, %eax
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
	movl %esi, %ebx
	movl %edi, %edi
	movl %ecx, %esi
	movl %edx, %edx
	movl %eax, %eax
	cmpl $1, %esi
	je L1_L_3
	jmp L1_L_2
L1_L_3:
	cmpl $1, %edx
	je L1_L_5
	jmp L1_L_4
L1_L_5:
	cmpl $1, %eax
	je L1_L_7
	jmp L1_L_6
L1_L_7:
	movl $15, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_6:
	movl $13, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_4:
	cmpl $1, %eax
	je L1_L_9
	jmp L1_L_8
L1_L_9:
	movl $11, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_8:
	movl $9, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_2:
	cmpl $1, %edx
	je L1_L_11
	jmp L1_L_10
L1_L_11:
	cmpl $1, %eax
	je L1_L_13
	jmp L1_L_12
L1_L_13:
	movl $7, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_12:
	movl $5, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_10:
	cmpl $1, %eax
	je L1_L_15
	jmp L1_L_14
L1_L_15:
	movl $3, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_L_14:
	movl $1, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
