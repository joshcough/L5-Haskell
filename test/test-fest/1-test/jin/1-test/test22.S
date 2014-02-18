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
	movl $16, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_mul
L1_Generated_Label_0:
	subl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_mul:
	cmpl $10, %rax
	jl L1_mul1
	jmp L1_mul2
L1_mul1:
	imull $2, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_mul2:
	movl $1, %rcx
L1_loop:
	cmpl $5, %rcx
	jl L1_times
	jmp L1_result
L1_times:
	imull %rcx, %rax
	addl $1, %rcx
	jmp L1_loop
L1_result:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
