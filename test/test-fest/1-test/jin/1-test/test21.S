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
	movl $18, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_add
L1_Generated_Label_0:
	subl $2, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_add:
	cmpl $20, %rax
	jl L1_add1
	jmp L1_add2
L1_add1:
	addl $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_add2:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_sub
L1_Generated_Label_1:
	addl $2, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_sub:
	subl $10, %rax
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
