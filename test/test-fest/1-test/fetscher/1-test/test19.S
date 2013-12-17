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
	pushl $3
	pushl $21
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $1, %edi
L1_loop:
	pushl %esi
	call print
	addl $4, %esp
	movl %edi, %eax
	imull $4, %eax
	addl %esi, %eax
	movl 0(%eax), %ecx
	addl $4, %eax
	movl 0(%eax), %edx
	sarl $1, %edx
	sarl $1, %ecx
	addl %ecx, %edx
	sall $1, %edx
	addl $1, %edx
	movl %edx, 0(%eax)
	addl $1, %edi
	cmpl $10, %edi
	jl L1_loop
	jmp L1_quit
L1_quit:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
