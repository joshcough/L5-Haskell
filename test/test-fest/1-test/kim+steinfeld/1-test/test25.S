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
	movl $5, %esi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_exp
L1_Generated_Label_0:
	movl %esi, %eax
	addl %eax, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_exp:
	subl $4, %esp
	movl %esi, -4(%ebp)
L1_loop:
	cmpl $1, %eax
	jg L1_keep_going
	jmp L1_done
L1_keep_going:
	subl $1, %eax
	movl -4(%ebp), %ebx
	imull %ebx, %esi
	jmp L1_loop
L1_done:
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
