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
	movl $2, %rax
	movl $2, %rbx
	movl $2, %rcx
L1_loop1:
L1_loop2:
	imull %rax, %rax
	subl $1, %rbx
	cmpl $0, %rbx
	je L1_finish2
	jmp L1_loop2
L1_finish2:
	movl $2, %rbx
	subl $1, %rcx
	cmpl $0, %rcx
	je L1_finish1
	jmp L1_loop1
L1_finish1:
	imull $2, %rax
	addl $1, %rax
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
