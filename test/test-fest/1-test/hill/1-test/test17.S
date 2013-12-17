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
	movl $L1_first_fn, %esi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp *%esi
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
L1_first_fn:
	movl $16, %esi
	sall $1, %esi
	addl $1, %esi
	pushl %esi
	call print
	addl $4, %esp
	movl $L1_second_fn, %ecx
	movl %ebp, %esp
	jmp *%ecx
L1_second_fn:
	movl $12, %esi
	sall $1, %esi
	addl $1, %esi
	pushl %esi
	call print
	addl $4, %esp
	movl $-201, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
