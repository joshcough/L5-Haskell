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
	addl $8, %rbx
	imull $2, %rsi
	addl $2, %rsi
	addl %rdi, %rsi
L1_loop:
	cmpl %rsi, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl -4(%rbx), %rdx
	addl $2, %rdx
	movl %rdx, 0(%rbx)
	addl $4, %rbx
	jmp L1_loop
L1_done:
	pushl %rdi
	call print
	addl $4, %esp
	movl %rdi, %rax
	movl $21, %rsi
	movl %rax, %rbx
	addl $8, %rbx
	imull $2, %rsi
	addl $2, %rsi
	addl %rdi, %rsi
L1_loop1:
	cmpl %rsi, %rbx
	jl L1_keep_going1
	jmp L1_done1
L1_keep_going1:
	movl -4(%rbx), %rdx
	movl 0(%rbx), %rcx
	subl %rcx, %rdx
	addl $1, %rdx
	movl %rdx, 0(%rbx)
	addl $4, %rbx
	jmp L1_loop1
L1_done1:
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
