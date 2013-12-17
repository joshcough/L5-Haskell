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
	movl $5, %ebx
	movl $5, %esi
	movl $-2, %edi
	cmp %esi, %ebx
	setle %al
	movzbl %al, %eax
	cmp %edi, %ebx
	setle %cl
	movzbl %cl, %ecx
	cmp %ebx, %edi
	setle %dl
	movzbl %dl, %edx
	movl %eax, %ebx
	movl %ecx, %esi
	movl %edx, %edi
	sall $1, %ebx
	addl $1, %ebx
	pushl %ebx
	call print
	addl $4, %esp
	sall $1, %esi
	addl $1, %esi
	pushl %esi
	call print
	addl $4, %esp
	sall $1, %edi
	addl $1, %edi
	pushl %edi
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
