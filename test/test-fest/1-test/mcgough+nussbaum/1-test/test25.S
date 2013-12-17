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
	movl $1, %eax
	movl $8, %edx
L1_top:
	imull %eax, %edx
	cmpl $67, %eax
	jl L1_increment
	jmp L1_edxcheck
L1_increment:
	addl $2, %eax
L1_edxcheck:
	cmpl $799, %edx
	jl L1_top
	jmp L1_done
L1_done:
	addl %edx, %eax
	pushl %eax
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
