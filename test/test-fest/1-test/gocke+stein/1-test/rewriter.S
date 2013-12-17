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
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, %edx
	movl $1024, %eax
	movl $0, %ebx
L1_loop:
	addl %eax, %ebx
	movl %ebx, 0(%edx)
	subl $4, %eax
	cmpl $0, %eax
	jl L1_finish
	jmp L1_loop
L1_finish:
	movl 0(%edx), %ecx
	imull $2, %ecx
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
