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
	movl $85, %eax
	movl $1, %ebx
L1_loop:
	cmpl %eax, %ebx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	addl $1, %ebx
	jmp L1_loop
L1_done:
	pushl %ebx
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
