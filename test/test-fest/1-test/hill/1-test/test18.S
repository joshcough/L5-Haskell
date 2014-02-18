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
	movl $25, %rbx
	pushl $1
	pushl %rbx
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $25, %rdi
	pushl $1
	pushl %rdi
	call allocate
	addl $8, %esp
	cmpl $28, %rdi
	jl L1__1
	jmp L1__2
L1__1:
	pushl $1
	pushl %rax
	call print_error
	addl $8, %esp
	jmp L1__3
L1__2:
	pushl %rax
	pushl $1
	call print_error
	addl $8, %esp
	jmp L1__3
L1__3:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
