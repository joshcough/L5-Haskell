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
	movl $21, %rsi
	pushl $3
	pushl %rsi
	call allocate
	addl $8, %esp
	movl %rax, %rdi
	movl %rdi, %rbx
	addl $12, %rbx
	imull $2, %rsi
	addl $2, %rsi
	addl %rdi, %rsi
L1_loop:
	subl $8, %esp
	cmpl %rsi, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl -4(%rbx), %rdx
	subl $1, %rdx
	movl %rdx, -4(%ebp)
	addl $1, %rdx
	imull %rdx, %rdx
	movl %rax, -8(%ebp)
	movl -4(%ebp), %rax
	imull %rax, %rax
	subl %rax, %rdx
	movl -8(%ebp), %rax
	movl %rdx, 0(%rbx)
	addl $4, %rbx
	addl $8, %esp
	jmp L1_loop
L1_done:
	pushl %rdi
	call print
	addl $4, %esp
	addl $8, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
