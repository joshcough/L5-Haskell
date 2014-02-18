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
	movl $5, %rsi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_exp
L1_Generated_Label_0:
	movl %rsi, %rax
	addl %rax, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_exp:
	subl $4, %esp
	movl %rsi, -4(%ebp)
L1_loop:
	cmpl $1, %rax
	jg L1_keep_going
	jmp L1_done
L1_keep_going:
	subl $1, %rax
	movl -4(%ebp), %rbx
	imull %rbx, %rsi
	jmp L1_loop
L1_done:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
