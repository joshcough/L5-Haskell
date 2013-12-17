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
	movl $0, %esi
L1_loop:
	movl %esi, %ecx
	imull $2, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	addl $3, %esi
	cmpl $10, %esi
	jl L1_loop
	jmp L1_out
L1_out:
	movl %eax, %eax
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
