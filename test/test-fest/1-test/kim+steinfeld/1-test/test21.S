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
	movl $21, %rdx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_factorial
L1_Generated_Label_0:
	pushl %rdi
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_factorial:
	movl %rdx, %rsi
	subl $1, %rsi
	sarl $1, %rsi
	movl $3, %rdi
	subl $1, %rdi
	sarl $1, %rdi
L1_loop:
	cmpl $1, %rsi
	jg L1_keep_going
	jmp L1_done
L1_keep_going:
	imull %rsi, %rdi
	subl $1, %rsi
	jmp L1_loop
L1_done:
	imull $2, %rdi
	addl $1, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
