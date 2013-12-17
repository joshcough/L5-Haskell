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
	movl $2, %eax
	movl $1, %ebx
	cmp %ebx, %eax
	setl %al
	movzbl %al, %eax
	movl $0, %ecx
	cmpl %ecx, %eax
	je L1_then
	jmp L1_else
L1_then:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_done
L1_else:
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
