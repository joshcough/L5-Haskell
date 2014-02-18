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
	movl $999, %rsi
	movl $999, %rdi
	cmpl %rdi, %rsi
	jl L1_true
	jmp L1_false
L1_true:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_done
L1_false:
	pushl $1
	call print
	addl $4, %esp
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
