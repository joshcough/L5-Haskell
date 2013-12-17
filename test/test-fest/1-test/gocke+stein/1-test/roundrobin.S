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
	movl $72, %eax
	movl %eax, %ebx
	movl %eax, %ecx
	movl %eax, %edx
	movl %eax, %esi
	movl %eax, %edi
	addl $1, %ebx
	movl %ebx, %eax
	movl %ebx, %ecx
	movl %ebx, %edx
	movl %ebx, %esi
	movl %ebx, %edi
	addl $1, %ecx
	movl %ecx, %eax
	movl %ecx, %ebx
	movl %ecx, %edx
	movl %ecx, %esi
	movl %ecx, %edi
	addl $1, %edx
	movl %edx, %eax
	movl %edx, %ebx
	movl %edx, %ecx
	movl %edx, %esi
	movl %edx, %edi
	addl $1, %esi
	movl %esi, %eax
	movl %esi, %ebx
	movl %esi, %ecx
	movl %esi, %edx
	movl %esi, %edi
	addl $1, %edi
	movl %edi, %eax
	movl %edi, %ebx
	movl %edi, %ecx
	movl %edi, %edx
	movl %edi, %esi
	imull $2, %eax
	addl $1, %eax
	imull $2, %ebx
	addl $1, %ebx
	imull $2, %ecx
	addl $1, %ecx
	imull $2, %edx
	addl $1, %edx
	imull $2, %esi
	addl $1, %esi
	imull $2, %edi
	addl $1, %edi
	pushl %eax
	call print
	addl $4, %esp
	pushl %esi
	call print
	addl $4, %esp
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
