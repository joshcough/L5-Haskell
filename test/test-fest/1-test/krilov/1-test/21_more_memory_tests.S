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
	pushl $0
	pushl $11
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	addl $4, %rsi
	movl %rsi, %rdi
	addl $16, %rdi
	movl $1, %rax
L1_loop1:
	cmpl $5, %rax
	jle L1_keep_going1
	jmp L1_done1
L1_keep_going1:
	movl %rax, %rbx
	imull $2, %rbx
	addl $1, %rbx
	movl %rbx, 0(%rsi)
	addl $4, %rsi
	addl $1, %rax
	jmp L1_loop1
L1_done1:
	subl $20, %rsi
L1_loop2:
	cmpl %rdi, %rsi
	jle L1_keep_going2
	jmp L1_done2
L1_keep_going2:
	movl 0(%rsi), %rax
	pushl %rax
	call print
	addl $4, %esp
	addl $4, %rsi
	jmp L1_loop2
L1_done2:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
