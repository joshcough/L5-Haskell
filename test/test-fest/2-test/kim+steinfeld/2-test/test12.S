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
	movl $30, %rax
	cmpl $31, %rax
	jle L1_first
	jmp L1_second
L1_first:
	sarl $1, %rax
	cmpl $15, %rax
	jle L1_third
	jmp L1_fourth
L1_second:
	movl $1, %rax
	jmp L1_end
L1_third:
	jmp L1_end
L1_fourth:
	movl $0, %rax
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
