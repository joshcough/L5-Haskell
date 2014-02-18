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
	pushl $3
	pushl $101
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	movl $50, %rbx
	movl $1, %rdx
L1_filler:
	movl %rbx, %rcx
	imull $4, %rcx
	addl %rdi, %rcx
	movl %rdx, %rsi
	imull $2, %rsi
	addl $1, %rsi
	movl %rsi, 0(%rcx)
	subl $1, %rbx
	addl $1, %rdx
	cmpl $1, %rbx
	jl L1_printer
	jmp L1_filler
L1_printer:
	pushl %rdi
	call print
	addl $4, %esp
L1_end:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
