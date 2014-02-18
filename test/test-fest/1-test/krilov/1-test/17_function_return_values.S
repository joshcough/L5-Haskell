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
	jmp L1_func2
L1_Generated_Label_0:
	pushl %rax
	call print
	addl $4, %esp
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_func3
L1_Generated_Label_1:
	pushl %rax
	call print
	addl $4, %esp
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_func1
L1_Generated_Label_2:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_func1:
	movl $3, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_func2:
	movl $5, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_func3:
	movl $7, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
