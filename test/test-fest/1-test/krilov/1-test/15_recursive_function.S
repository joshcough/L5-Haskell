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
	movl $3, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_rec
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_rec:
	pushl $3
	call print
	addl $4, %esp
	subl $1, %rdi
	cmpl $0, %rdi
	jle L1_done
	jmp L1_recur
L1_recur:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_rec
L1_Generated_Label_1:
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
