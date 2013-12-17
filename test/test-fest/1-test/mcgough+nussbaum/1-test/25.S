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
	pushl $3
	pushl $5
	call allocate
	addl $8, %esp
	movl %eax, %ecx
	movl $6, 0(%ecx)
	movl $1, 4(%ecx)
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_function
L1_Generated_Label_0:
	movl 4(%ecx), %eax
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
L1_function:
	movl 0(%ecx), %edx
	cmpl $1, %edx
	jg L1_recur
	jmp L1_done
L1_recur:
	movl %edx, %esi
	sarl $1, %esi
	movl %esi, 0(%ecx)
	movl 4(%ecx), %esi
	sall $1, %esi
	movl %esi, 4(%ecx)
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_function
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
