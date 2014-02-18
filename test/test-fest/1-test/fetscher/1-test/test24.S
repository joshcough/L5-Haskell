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
	jmp L1_one
L1_Generated_Label_0:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_two
L1_Generated_Label_1:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_three
L1_Generated_Label_2:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_one:
	movl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_two:
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_one
L1_Generated_Label_3:
	movl %ebp, %esp
	popl %ebp
	ret
L1_three:
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_two
L1_Generated_Label_4:
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_one
L1_Generated_Label_5:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
