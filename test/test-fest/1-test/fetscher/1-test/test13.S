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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_a
L1_Generated_Label_0:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_a:
	jmp L1_true
L1_true:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_b
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_false:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_c
L1_Generated_Label_2:
	movl %ebp, %esp
	popl %ebp
	ret
L1_b:
	jmp L1_trueb
L1_trueb:
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_d
L1_Generated_Label_3:
	movl %ebp, %esp
	popl %ebp
	ret
L1_falseb:
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_e
L1_Generated_Label_4:
	movl %ebp, %esp
	popl %ebp
	ret
L1_c:
	movl $0, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_d:
	jmp L1_falsed
L1_trued:
	movl $0, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_falsed:
	movl $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_e:
	movl $0, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
