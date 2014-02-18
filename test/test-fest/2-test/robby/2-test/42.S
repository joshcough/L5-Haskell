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
	addl $-20, %esp
	movl $3, %rsi
	movl $5, %rdi
	movl $7, -20(%ebp)
	movl $9, -16(%ebp)
	movl $11, -12(%ebp)
	movl $13, -8(%ebp)
	movl $15, -4(%ebp)
	movl $1, %rcx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
L1_Generated_Label_0:
	pushl %rsi
	call print
	addl $4, %esp
	pushl %rdi
	call print
	addl $4, %esp
	movl -20(%ebp), %rbx
	pushl %rbx
	call print
	addl $4, %esp
	movl -16(%ebp), %rbx
	pushl %rbx
	call print
	addl $4, %esp
	movl -12(%ebp), %rbx
	pushl %rbx
	call print
	addl $4, %esp
	movl -8(%ebp), %rbx
	pushl %rbx
	call print
	addl $4, %esp
	movl -4(%ebp), %rbx
	pushl %rbx
	call print
	addl $4, %esp
	subl $-20, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_f:
	movl %rsi, %rsi
	movl %rdi, %rdi
	movl %rcx, %rbx
L1_top:
	cmpl $0, %rbx
	je L1_return
	jmp L1_loop
L1_return:
	movl %rsi, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
L1_loop:
	subl $1, %rbx
	movl $3, %rdx
	movl $5, %rdx
	movl $7, %rdx
	movl $9, %rdx
	movl $11, %rdx
	movl $13, %rdx
	movl $15, %rdx
	jmp L1_top
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
