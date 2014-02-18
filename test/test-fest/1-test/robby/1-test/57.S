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
	movl $3, %rax
	cmpl $3, %rax
	je L1_then
	jmp L1_else
L1_then:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_done
L1_else:
	pushl $1
	call print
	addl $4, %esp
L1_done:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
