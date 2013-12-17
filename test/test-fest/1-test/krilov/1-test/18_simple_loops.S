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
	movl $5, %edi
	movl $0, %esi
L1_loop:
	cmpl %edi, %esi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	pushl $3
	call print
	addl $4, %esp
	addl $1, %esi
	jmp L1_loop
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
