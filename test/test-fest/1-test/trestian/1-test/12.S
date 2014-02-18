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
	movl $-3, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L1
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_L1:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L2
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L2:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L3
L1_Generated_Label_2:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L3:
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L4
L1_Generated_Label_3:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L4:
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L5
L1_Generated_Label_4:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L5:
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L6
L1_Generated_Label_5:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L6:
	pushl $L1_Generated_Label_6
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L7
L1_Generated_Label_6:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L7:
	pushl $L1_Generated_Label_7
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L8
L1_Generated_Label_7:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L8:
	pushl $L1_Generated_Label_8
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L9
L1_Generated_Label_8:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L9:
	pushl $L1_Generated_Label_9
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L10
L1_Generated_Label_9:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L10:
	pushl $L1_Generated_Label_10
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L11
L1_Generated_Label_10:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L11:
	pushl $L1_Generated_Label_11
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L12
L1_Generated_Label_11:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L12:
	pushl $L1_Generated_Label_12
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L13
L1_Generated_Label_12:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L13:
	pushl $L1_Generated_Label_13
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L14
L1_Generated_Label_13:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L14:
	pushl $L1_Generated_Label_14
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L15
L1_Generated_Label_14:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L15:
	pushl $L1_Generated_Label_15
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L16
L1_Generated_Label_15:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L16:
	pushl $L1_Generated_Label_16
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L17
L1_Generated_Label_16:
	movl %ebp, %esp
	popl %ebp
	ret
L1_L17:
	pushl %rdi
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
