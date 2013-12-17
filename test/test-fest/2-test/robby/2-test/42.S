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
	addl $-20, %esp
	movl $3, %esi
	movl $5, %edi
	movl $7, -20(%ebp)
	movl $9, -16(%ebp)
	movl $11, -12(%ebp)
	movl $13, -8(%ebp)
	movl $15, -4(%ebp)
	movl $1, %ecx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_f
L1_Generated_Label_0:
	pushl %esi
	call print
	addl $4, %esp
	pushl %edi
	call print
	addl $4, %esp
	movl -20(%ebp), %ebx
	pushl %ebx
	call print
	addl $4, %esp
	movl -16(%ebp), %ebx
	pushl %ebx
	call print
	addl $4, %esp
	movl -12(%ebp), %ebx
	pushl %ebx
	call print
	addl $4, %esp
	movl -8(%ebp), %ebx
	pushl %ebx
	call print
	addl $4, %esp
	movl -4(%ebp), %ebx
	pushl %ebx
	call print
	addl $4, %esp
	subl $-20, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_f:
	movl %esi, %esi
	movl %edi, %edi
	movl %ecx, %ebx
L1_top:
	cmpl $0, %ebx
	je L1_return
	jmp L1_loop
L1_return:
	movl %esi, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
L1_loop:
	subl $1, %ebx
	movl $3, %edx
	movl $5, %edx
	movl $7, %edx
	movl $9, %edx
	movl $11, %edx
	movl $13, %edx
	movl $15, %edx
	jmp L1_top
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
