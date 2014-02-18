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
	movl $85, %rax
	movl $1, %rbx
L1_loop:
	cmpl $0, %rax
	je L1_done
	jmp L1_keep_going
L1_keep_going:
	subl $1, %rax
	addl $3, %rbx
	jmp L1_loop
L1_done:
	movl %rbx, %rcx
L1_loop2:
	cmpl $42, %rax
	jl L1_done2
	jmp L1_keep_going2
L1_keep_going2:
	imull %rbx, %rcx
	subl $1, %rbx
	jmp L1_loop2
L1_done2:
	imull $2, %rcx
	addl $1, %rcx
	pushl %rcx
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
