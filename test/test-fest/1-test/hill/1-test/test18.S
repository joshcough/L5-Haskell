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
	movl $25, %ebx
	pushl $1
	pushl %ebx
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $25, %edi
	pushl $1
	pushl %edi
	call allocate
	addl $8, %esp
	cmpl $28, %edi
	jl L1__1
	jmp L1__2
L1__1:
	pushl $1
	pushl %eax
	call print_error
	addl $8, %esp
	jmp L1__3
L1__2:
	pushl %eax
	pushl $1
	call print_error
	addl $8, %esp
	jmp L1__3
L1__3:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
