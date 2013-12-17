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
	pushl $1
	pushl $3
	call allocate
	addl $8, %esp
	movl %eax, %ebx
	addl $4, %ebx
	cmpl %eax, %ebx
	jl L1_lt
	jmp L1_ge
L1_lt:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_done
L1_ge:
	pushl $1
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
