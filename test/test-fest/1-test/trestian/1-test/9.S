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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_first
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_first:
	movl $3, %rdi
	movl $51, %rsi
	pushl %rdi
	call print
	addl $4, %esp
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_second
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_second:
	addl $2, %rdi
	pushl %rdi
	call print
	addl $4, %esp
	cmpl %rsi, %rdi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_second
L1_Generated_Label_2:
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
