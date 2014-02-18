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
	movl $2147383647, %rbx
	movl $-2147483647, %rdx
L1_loop:
	cmpl %rbx, %rdx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	addl $100000, %rdx
	jmp L1_loop
L1_done:
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
