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
	movl $10, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_0:
	movl $7, %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_1:
	movl $9, %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_2:
	movl $6, %eax
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_3:
	movl $11, %eax
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_4:
	movl $8, %eax
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_FACTORIAL
L1_Generated_Label_5:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_FACTORIAL:
	movl %eax, %esi
	movl $0, %edi
	movl $1, %eax
L1_RERUN:
	addl $1, %edi
	imull %edi, %eax
	cmpl %esi, %edi
	jl L1_RERUN
	jmp L1_ANSWER
L1_ANSWER:
	sall $1, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $0, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
