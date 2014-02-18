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
	movl $15, %rsi
	movl $25, %rbx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_helper
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_helper:
	pushl %rdi
	call print
	addl $4, %esp
	pushl %rsi
	call print
	addl $4, %esp
	cmpl %rsi, %rdi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl %ebp, %esp
	popl %ebp
	ret
L1_done:
	pushl %rbx
	call print
	addl $4, %esp
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
