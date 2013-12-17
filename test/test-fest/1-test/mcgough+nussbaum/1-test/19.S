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
	pushl $3
	call allocate
	addl $8, %esp
	movl %eax, %ecx
	movl $21, 0(%ecx)
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_function1
L1_Generated_Label_0:
	movl 0(%ecx), %eax
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_function1:
	movl 0(%ecx), %edx
	cmpl $23, %edx
	jl L1_good
	jmp L1_bad
L1_good:
	movl %edx, %eax
	jmp L1_return
L1_bad:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_function2
L1_Generated_Label_1:
	jmp L1_return
L1_return:
	movl %ebp, %esp
	popl %ebp
	ret
L1_function2:
	addl $2, %edx
	movl %edx, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
