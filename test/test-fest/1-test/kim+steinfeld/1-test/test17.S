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
	movl $7, %rsi
	pushl $5
	pushl %rsi
	call allocate
	addl $8, %esp
	movl $15, 4(%rax)
	movl $21, 8(%rax)
	movl 4(%rax), %rbx
	movl 8(%rax), %rdx
	cmpl %rdx, %rbx
	jl L1_if
	jmp L1_else
L1_if:
	movl $51, 12(%rax)
	jmp L1_done
L1_else:
	movl $101, 12(%rax)
L1_done:
	movl 12(%rax), %rdx
	pushl %rdx
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
