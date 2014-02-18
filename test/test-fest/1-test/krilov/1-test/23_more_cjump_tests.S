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
	movl $3, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_func1
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_func1:
	pushl $3
	call print
	addl $4, %esp
	subl $1, %rdi
	cmpl $0, %rdi
	jle L1_call_func2
	jmp L1_recur
L1_recur:
	movl %ebp, %esp
	jmp L1_func1
L1_call_func2:
	movl %ebp, %esp
	jmp L1_func2
	movl %ebp, %esp
	popl %ebp
	ret
L1_func2:
	pushl $5
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
