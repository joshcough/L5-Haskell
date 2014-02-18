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
	pushl $5
	pushl $5
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $10, %rdi
	pushl %rsi
	call print
	addl $4, %esp
L1_loop:
	movl 0(%rsi), %rax
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	pushl %rax
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	subl $1, %rdi
	cmpl $0, %rdi
	jg L1_loop
	jmp L1_exit
L1_exit:
	pushl %rsi
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
