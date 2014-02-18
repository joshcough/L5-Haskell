	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %rbx
	pushl %rsi
	pushl %rdi
	pushl %ebp
	movl	%esp, %ebp
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_numberone
L1_Generated_Label_0:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_numberone:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_numbertwo
L1_Generated_Label_1:
	addl $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_numbertwo:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_numberthree
L1_Generated_Label_2:
	sall $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_numberthree:
	movl $9, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
