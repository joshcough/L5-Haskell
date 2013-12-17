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
	jmp L1_alloc
L1_Generated_Label_0:
	movl %eax, %esi
	movl 0(%esi), %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printnum
L1_Generated_Label_1:
	movl 4(%esi), %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printnum
L1_Generated_Label_2:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_alloc:
	pushl $7
	pushl $7
	call allocate
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_printnum:
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
