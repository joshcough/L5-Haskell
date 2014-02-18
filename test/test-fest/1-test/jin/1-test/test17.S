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
	movl $15, %rcx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_fib:
L1_loop:
	cmpl %rsi, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	cmpl $6, %rcx
	jl L1_base
	jmp L1_recure
L1_base:
	movl -4(%rbx), %rdx
	imull $2, %rdx
	addl $1, %rdx
	movl %rdx, 0(%rbx)
	addl $4, %rbx
	movl %ebp, %esp
	popl %ebp
	ret
L1_recure:
	subl $1, %rcx
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_1:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib1
L1_Generated_Label_2:
	jmp L1_loop
	movl %ebp, %esp
	popl %ebp
	ret
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
L1_fib1:
	movl -4(%rbx), %rdx
	addl $2, %rdx
	movl %rdx, -4(%rbx)
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
