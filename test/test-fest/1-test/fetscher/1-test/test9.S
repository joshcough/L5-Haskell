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
	movl $1, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printone
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_printone:
	movl %rax, %rsi
	sall $1, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl %rsi, %rax
	sarl $1, %rax
	addl $1, %rax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_skipone
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_skipone:
	cmpl $10, %rax
	jl L1_go_on
	jmp L1_quit
L1_go_on:
	addl $1, %rax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printone
L1_Generated_Label_2:
L1_quit:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
