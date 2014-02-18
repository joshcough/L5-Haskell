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
	movl $5, %rdi
	movl $-5, %rsi
L1_loop1:
	cmpl %rdi, %rsi
	jle L1_keep_going1
	jmp L1_done1
L1_keep_going1:
	cmp $2, %rsi
	sete %al
	movzbl %al, %rax
	imull $2, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	addl $1, %rsi
	jmp L1_loop1
L1_done1:
	movl $5, %rdi
	movl $-5, %rsi
L1_loop2:
	cmpl %rdi, %rsi
	jle L1_keep_going2
	jmp L1_done2
L1_keep_going2:
	cmp $2, %rsi
	setl %al
	movzbl %al, %rax
	imull $2, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	addl $1, %rsi
	jmp L1_loop2
L1_done2:
	movl $5, %rdi
	movl $-5, %rsi
L1_loop3:
	cmpl %rdi, %rsi
	jle L1_keep_going3
	jmp L1_done3
L1_keep_going3:
	cmp $2, %rsi
	setle %al
	movzbl %al, %rax
	imull $2, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	addl $1, %rsi
	jmp L1_loop3
L1_done3:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
