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
	pushl $15
	pushl $11
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	addl $4, %rsi
	movl %rsi, %rdi
	addl $16, %rdi
L1_loop:
	cmpl %rdi, %rsi
	jle L1_keep_going
	jmp L1_done
L1_keep_going:
	movl 0(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	addl $4, %rsi
	jmp L1_loop
L1_done:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
