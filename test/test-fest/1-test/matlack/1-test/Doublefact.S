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
	movl %rax, %rcx
	subl $1, %rcx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fact
L1_Generated_Label_0:
	movl %rax, %rcx
	subl $1, %rcx
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fact
L1_Generated_Label_1:
	imull $2, %rax
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
L1_fact:
L1_loop:
	imull %rcx, %rax
	subl $1, %rcx
	cmpl $0, %rcx
	je L1_done
	jmp L1_loop
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
