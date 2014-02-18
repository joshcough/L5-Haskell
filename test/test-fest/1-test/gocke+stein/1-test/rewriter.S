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
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, %rdx
	movl $1024, %rax
	movl $0, %rbx
L1_loop:
	addl %rax, %rbx
	movl %rbx, 0(%rdx)
	subl $4, %rax
	cmpl $0, %rax
	jl L1_finish
	jmp L1_loop
L1_finish:
	movl 0(%rdx), %rcx
	imull $2, %rcx
	addl $1, %rcx
	pushl %rcx
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
