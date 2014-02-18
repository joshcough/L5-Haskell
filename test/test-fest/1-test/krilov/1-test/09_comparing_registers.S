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
	cmp $3, %rax
	sete %al
	movzbl %al, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $4, %rbx
	cmp $3, %rbx
	sete %al
	movzbl %al, %rax
	addl $11, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $5, %rax
	movl $7, %rbx
	cmp %rbx, %rax
	setl %al
	movzbl %al, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $5, %rax
	cmp $6, %rax
	setle %al
	movzbl %al, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $5, %rbx
	cmp $7, %rbx
	setg %al
	movzbl %al, %rax
	addl $11, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $6, %rbx
	cmp %rbx, %rbx
	setl %al
	movzbl %al, %rax
	addl $11, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl $6, %rax
	cmp $6, %rax
	setle %al
	movzbl %al, %rax
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
