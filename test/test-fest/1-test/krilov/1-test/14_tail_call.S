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
	jmp L1_func1
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
L1_func1:
	pushl $3
	call print
	addl $4, %esp
	movl %ebp, %esp
	jmp L1_func3
L1_func2:
	movl $5, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_func3:
	pushl $7
	call print
	addl $4, %esp
	movl %ebp, %esp
	jmp L1_func2
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
