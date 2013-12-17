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
	movl $3, %eax
	cmp $3, %eax
	sete %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $4, %ebx
	cmp $3, %ebx
	sete %al
	movzbl %al, %eax
	addl $11, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $5, %eax
	movl $7, %ebx
	cmp %ebx, %eax
	setl %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $5, %eax
	cmp $6, %eax
	setle %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $5, %ebx
	cmp $7, %ebx
	setg %al
	movzbl %al, %eax
	addl $11, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $6, %ebx
	cmp %ebx, %ebx
	setl %al
	movzbl %al, %eax
	addl $11, %eax
	pushl %eax
	call print
	addl $4, %esp
	movl $6, %eax
	cmp $6, %eax
	setle %al
	movzbl %al, %eax
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
