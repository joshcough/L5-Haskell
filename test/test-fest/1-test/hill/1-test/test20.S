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
	movl $1, %rdx
	movl %rdx, %rdi
	cmpl $0, %rdi
	jl L1_a
	jmp L1_a
	pushl $25
	call print
	addl $4, %esp
L1_a:
	cmpl $0, %rdi
	jle L1_b
	jmp L1_b
	pushl $39
	call print
	addl $4, %esp
L1_b:
	cmpl $0, %rdi
	je L1_c
	jmp L1_c
	pushl $35
	call print
	addl $4, %esp
L1_c:
	cmpl $1, %rdi
	je L1_d
	jmp L1_d
	pushl $81
	call print
	addl $4, %esp
L1_d:
	pushl $95
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
