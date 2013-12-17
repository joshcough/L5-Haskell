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
	movl $-5, %esi
L1_loop1:
	cmpl %edi, %esi
	jle L1_keep_going1
	jmp L1_done1
L1_keep_going1:
	cmp $2, %esi
	sete %al
	movzbl %al, %eax
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	addl $1, %esi
	jmp L1_loop1
L1_done1:
	movl $5, %edi
	movl $-5, %esi
L1_loop2:
	cmpl %edi, %esi
	jle L1_keep_going2
	jmp L1_done2
L1_keep_going2:
	cmp $2, %esi
	setl %al
	movzbl %al, %eax
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	addl $1, %esi
	jmp L1_loop2
L1_done2:
	movl $5, %edi
	movl $-5, %esi
L1_loop3:
	cmpl %edi, %esi
	jle L1_keep_going3
	jmp L1_done3
L1_keep_going3:
	cmp $2, %esi
	setle %al
	movzbl %al, %eax
	imull $2, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	addl $1, %esi
	jmp L1_loop3
L1_done3:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
