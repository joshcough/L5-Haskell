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
	movl $-1, %rsi
	pushl %rsi
	call print
	addl $4, %esp
	movl $-3, %rdi
	pushl %rdi
	call print
	addl $4, %esp
	cmpl %rsi, %rdi
	jl L1_end
	jmp L1_bad
L1_bad:
	pushl %rsi
	call print
	addl $4, %esp
L1_end:
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
