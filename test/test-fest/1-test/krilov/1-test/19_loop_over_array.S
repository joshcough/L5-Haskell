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
	pushl $15
	pushl $11
	call allocate
	addl $8, %esp
	movl %eax, %esi
	addl $4, %esi
	movl %esi, %edi
	addl $16, %edi
L1_loop:
	cmpl %edi, %esi
	jle L1_keep_going
	jmp L1_done
L1_keep_going:
	movl 0(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	addl $4, %esi
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
