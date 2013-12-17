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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_go
L1_Generated_Label_0:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_go:
	movl %eax, %ebx
	addl %ebx, %ebx
	movl %eax, %ecx
	addl $2, %ecx
	cmpl %ecx, %ebx
	jl L1_true
	jmp L1_false
L1_true:
	movl $15, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_false:
	movl %ecx, %eax
	imull %ecx, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
