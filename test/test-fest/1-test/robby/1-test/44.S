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
	pushl $1
	pushl $3
	call allocate
	addl $8, %esp
	movl %rax, %rbx
	addl $4, %rbx
	cmpl %rax, %rbx
	jl L1_lt
	jmp L1_ge
L1_lt:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_done
L1_ge:
	pushl $1
	call print
	addl $4, %esp
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
