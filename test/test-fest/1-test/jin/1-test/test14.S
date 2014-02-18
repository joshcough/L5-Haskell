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
	movl $1, %rbx
	movl $3, %rax
L1_loop:
	cmpl $5, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	cmpl $1, %rax
	je L1_step_1
	jmp L1_step_2
L1_step_1:
	addl $10, %rax
	addl $1, %rbx
	jmp L1_loop
L1_step_2:
	addl $4, %rax
	addl $1, %rbx
	jmp L1_loop
L1_done:
	pushl %rax
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
