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
	movl $25, %rcx
	movl $3, %rbx
L1_loop:
	cmpl %rcx, %rbx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl %rbx, %rdx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_func
L1_Generated_Label_0:
	movl %rdx, %rbx
	addl $2, %rbx
L1_done:
	pushl %rbx
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_func:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
