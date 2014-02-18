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
	jmp L1_alloc
L1_Generated_Label_0:
	movl %rax, %rsi
	movl 0(%rsi), %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printnum
L1_Generated_Label_1:
	movl 4(%rsi), %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printnum
L1_Generated_Label_2:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_alloc:
	pushl $7
	pushl $7
	call allocate
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_printnum:
	imull $2, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
