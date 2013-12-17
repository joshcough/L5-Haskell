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
	movl $7, %esi
	pushl $5
	pushl %esi
	call allocate
	addl $8, %esp
	movl $15, 4(%eax)
	movl $21, 8(%eax)
	movl 4(%eax), %ebx
	movl 8(%eax), %edx
	cmpl %edx, %ebx
	jl L1_if
	jmp L1_else
L1_if:
	movl $51, 12(%eax)
	jmp L1_done
L1_else:
	movl $101, 12(%eax)
L1_done:
	movl 12(%eax), %edx
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
