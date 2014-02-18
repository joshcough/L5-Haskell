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
	movl $4, %rax
	movl %rax, %rbx
	movl $13, %rcx
L1_loop:
	cmpl $1, %rcx
	je L1_done
	jmp L1_keep_going
L1_keep_going:
	imull %rbx, %rax
	subl $1, %rcx
	jmp L1_loop
L1_done:
	sall $1, %rax
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
