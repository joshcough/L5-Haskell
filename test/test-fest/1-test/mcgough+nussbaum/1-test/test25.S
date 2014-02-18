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
	movl $1, %rax
	movl $8, %rdx
L1_top:
	imull %rax, %rdx
	cmpl $67, %rax
	jl L1_increment
	jmp L1_rdxcheck
L1_increment:
	addl $2, %rax
L1_rdxcheck:
	cmpl $799, %rdx
	jl L1_top
	jmp L1_done
L1_done:
	addl %rdx, %rax
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
