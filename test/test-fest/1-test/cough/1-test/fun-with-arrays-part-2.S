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
	movl $50, %rsi
	movl $1, %rdx
L1_filler:
	movl %rsi, %rcx
	imull $4, %rcx
	addl %rdi, %rcx
	movl %rdx, %rbx
	imull $2, %rbx
	addl $1, %rbx
	movl %rbx, 0(%rcx)
	subl $1, %rsi
	addl $1, %rdx
	cmpl $1, %rsi
	jl L1_printer
	jmp L1_filler
L1_printer:
	movl $1, %rsi
L1_printer2:
	movl %rsi, %rcx
	imull $4, %rcx
	addl %rdi, %rcx
	movl 0(%rcx), %rcx
	pushl %rcx
	call print
	addl $4, %esp
	movl $51, %rdx
	addl $1, %rsi
	cmpl %rdx, %rsi
	jl L1_printer2
	jmp L1_ender
L1_ender:
	pushl %rdi
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
