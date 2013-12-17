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
	movl $10, %ecx
	movl $3, %esi
	movl $23, %eax
	movl $15, %ebx
	cmpl %ebx, %eax
	je L1_first
	jmp L1_second
L1_first:
	movl %eax, %ebx
	movl %ebx, %ecx
	jmp L1_end
L1_second:
	addl %ebx, %ebx
L1_end:
	addl %ebx, %eax
	imull %eax, %ecx
	pushl %eax
	pushl %esi
	call allocate
	addl $8, %esp
	movl 4(%eax), %edx
	pushl %edx
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
