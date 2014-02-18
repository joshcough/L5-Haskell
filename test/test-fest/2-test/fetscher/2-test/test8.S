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
	movl $-1, %rax
	movl %rax, %rcx
	sarl $1, %rcx
	andl %rcx, %rax
	cmpl $0, %rax
	jl L1_yes
	jmp L1_no
L1_yes:
	pushl %rax
	call print
	addl $4, %esp
	jmp L1_end
L1_no:
	pushl %rcx
	call print
	addl $4, %esp
L1_end:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
