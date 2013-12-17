	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %ebx
	pushl %esi
	pushl %edi
	pushl %ebp
	movl	%esp, %ebp
	movl $1, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_foo
L1_Generated_Label_0:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_foo
L1_Generated_Label_1:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_foo
L1_Generated_Label_2:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_foo:
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_bar
L1_Generated_Label_3:
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_bar
L1_Generated_Label_4:
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_bar
L1_Generated_Label_5:
	movl %ebp, %esp
	popl %ebp
	ret
L1_bar:
	pushl $L1_Generated_Label_6
	pushl %ebp
	movl %esp, %ebp
	jmp L1_baz
L1_Generated_Label_6:
	pushl $L1_Generated_Label_7
	pushl %ebp
	movl %esp, %ebp
	jmp L1_qux
L1_Generated_Label_7:
	pushl $L1_Generated_Label_8
	pushl %ebp
	movl %esp, %ebp
	jmp L1_quux
L1_Generated_Label_8:
	movl %ebp, %esp
	popl %ebp
	ret
L1_baz:
	pushl $L1_Generated_Label_9
	pushl %ebp
	movl %esp, %ebp
	jmp L1_qux
L1_Generated_Label_9:
	pushl $L1_Generated_Label_10
	pushl %ebp
	movl %esp, %ebp
	jmp L1_qux
L1_Generated_Label_10:
	movl %ebp, %esp
	popl %ebp
	ret
L1_qux:
	pushl $L1_Generated_Label_11
	pushl %ebp
	movl %esp, %ebp
	jmp L1_quux
L1_Generated_Label_11:
	movl %ebp, %esp
	popl %ebp
	ret
L1_quux:
	addl $8, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
