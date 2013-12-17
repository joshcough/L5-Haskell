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
	pushl $0
	pushl $11
	call allocate
	addl $8, %esp
	movl %eax, %esi
	addl $4, %esi
	movl %esi, %edi
	addl $16, %edi
	movl $1, %eax
L1_loop1:
	cmpl $5, %eax
	jle L1_keep_going1
	jmp L1_done1
L1_keep_going1:
	movl %eax, %ebx
	imull $2, %ebx
	addl $1, %ebx
	movl %ebx, 0(%esi)
	addl $4, %esi
	addl $1, %eax
	jmp L1_loop1
L1_done1:
	subl $20, %esi
L1_loop2:
	cmpl %edi, %esi
	jle L1_keep_going2
	jmp L1_done2
L1_keep_going2:
	movl 0(%esi), %eax
	pushl %eax
	call print
	addl $4, %esp
	addl $4, %esi
	jmp L1_loop2
L1_done2:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
