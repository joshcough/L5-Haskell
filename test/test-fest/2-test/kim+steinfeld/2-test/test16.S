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
	movl $20, %rbx
	movl $20, %rcx
	cmp %rcx, %rbx
	setl %al
	movzbl %al, %rax
	cmp %rbx, %rcx
	setle %cl
	movzbl %cl, %rcx
	cmp %rbx, %rbx
	sete %cl
	movzbl %cl, %rcx
	cmp %rbx, %rax
	setle %bl
	movzbl %bl, %rbx
	cmp %rax, %rbx
	sete %bl
	movzbl %bl, %rbx
	imull $2, %rbx
	addl $1, %rbx
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
