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
	movl $0, %eax
	movl $2097152, %ebx
L1_loop:
	addl %ebx, %eax
	cmpl $2145386495, %eax
	jl L1_loop
	jmp L1_finish
L1_finish:
	movl %eax, %ecx
	addl $1, %ecx
	pushl %ecx
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
