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
	movl $L1_first_fn, %rsi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp *%rsi
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
L1_first_fn:
	movl $16, %rsi
	sall $1, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $L1_second_fn, %rcx
	movl %ebp, %esp
	jmp *%rcx
L1_second_fn:
	movl $12, %rsi
	sall $1, %rsi
	addl $1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $-201, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
