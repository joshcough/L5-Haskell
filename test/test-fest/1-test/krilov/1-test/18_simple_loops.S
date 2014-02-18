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
	movl $5, %rdi
	movl $0, %rsi
L1_loop:
	cmpl %rdi, %rsi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	pushl $3
	call print
	addl $4, %esp
	addl $1, %rsi
	jmp L1_loop
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
