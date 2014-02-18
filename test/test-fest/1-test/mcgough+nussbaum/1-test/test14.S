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
	movl $1, %rdx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_bar
L1_Generated_Label_0:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_foo:
	pushl $1
	pushl $5
	call allocate
	addl $8, %esp
	movl $53, 4(%rax)
	movl $1, 0(%rax)
	movl 0(%rax), %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_bar:
	movl %rdx, %rax
	addl $2, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
