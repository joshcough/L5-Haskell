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
	movl $30, %eax
	cmpl $31, %eax
	jle L1_first
	jmp L1_second
L1_first:
	sarl $1, %eax
	cmpl $15, %eax
	jle L1_third
	jmp L1_fourth
L1_second:
	movl $1, %eax
	jmp L1_end
L1_third:
	jmp L1_end
L1_fourth:
	movl $0, %eax
L1_end:
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
