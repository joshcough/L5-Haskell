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
	movl $65, %rax
	movl $15, %rbx
	jmp L1_loop2
L1_loop1:
	addl $327, %rax
	subl $1, %rax
L1_loop2:
	subl $3, %rax
	addl $1, %rax
	subl $3, %rbx
	addl $1, %rbx
	cmpl $1, %rbx
	je L1_end
	jmp L1_loop2
L1_end:
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
