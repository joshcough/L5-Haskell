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
	movl $11, %edx
	movl $21, %ecx
	cmpl %edx, %ecx
	jl L1_if
	jmp L1_else
L1_if:
	pushl %edx
	call print
	addl $4, %esp
	jmp L1_done
L1_else:
	pushl %ecx
	call print
	addl $4, %esp
L1_done:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
