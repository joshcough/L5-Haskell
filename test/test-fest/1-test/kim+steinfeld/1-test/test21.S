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
	movl $21, %edx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_factorial
L1_Generated_Label_0:
	pushl %edi
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_factorial:
	movl %edx, %esi
	subl $1, %esi
	sarl $1, %esi
	movl $3, %edi
	subl $1, %edi
	sarl $1, %edi
L1_loop:
	cmpl $1, %esi
	jg L1_keep_going
	jmp L1_done
L1_keep_going:
	imull %esi, %edi
	subl $1, %esi
	jmp L1_loop
L1_done:
	imull $2, %edi
	addl $1, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
