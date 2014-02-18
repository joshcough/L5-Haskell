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
	movl $4, %rsi
	movl $0, %rdi
	movl $1, %rbx
L1_top:
	cmpl %rdi, %rsi
	je L1_done
	jmp L1_cont
L1_cont:
	pushl %rbx
	pushl $3
	call allocate
	addl $8, %esp
	movl %rax, %rbx
	subl $1, %rsi
	jmp L1_top
L1_done:
	pushl %rbx
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
