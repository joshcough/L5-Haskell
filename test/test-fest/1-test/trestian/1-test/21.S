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
	movl $L1_func1, %rsi
	movl $1, %rdi
L1_loop:
	cmpl $5, %rdi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	addl $1, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp *%rsi
L1_Generated_Label_0:
	jmp L1_loop
L1_done:
	pushl %rdi
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_func1:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
