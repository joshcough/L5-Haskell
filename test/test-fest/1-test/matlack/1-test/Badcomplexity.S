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
	movl $5, %eax
	movl %eax, %ecx
	subl $1, %ecx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_bad
L1_Generated_Label_0:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_complexity
L1_Generated_Label_1:
	imull $2, %eax
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
L1_bad:
L1_loop:
	imull %ecx, %eax
	subl $1, %ecx
	cmpl $0, %ecx
	je L1_done
	jmp L1_loop
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
L1_complexity:
	imull %eax, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
