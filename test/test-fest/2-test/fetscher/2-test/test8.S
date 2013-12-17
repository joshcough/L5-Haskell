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
	movl $-1, %eax
	movl %eax, %ecx
	sarl $1, %ecx
	andl %ecx, %eax
	cmpl $0, %eax
	jl L1_yes
	jmp L1_no
L1_yes:
	pushl %eax
	call print
	addl $4, %esp
	jmp L1_end
L1_no:
	pushl %ecx
	call print
	addl $4, %esp
L1_end:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
