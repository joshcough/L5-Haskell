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
	movl $3, %rdi
	pushl %rdi
	call print
	addl $4, %esp
	movl $71, %rsi
	movl $13, %rbx
L1_loop:
	cmpl %rsi, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	sall $1, %rdi
	addl $1, %rdi
	addl $2, %rbx
	jmp L1_loop
L1_done:
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
