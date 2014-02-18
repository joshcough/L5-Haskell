	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %rbx
	pushl %rsi
	pushl %rdi
	pushl %ebp
	movl	%esp, %ebp
	movl $1234567, %rax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_zeroit
L1_Generated_Label_0:
L1_fakereturn:
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_zeroit:
	movl $0, %rbx
	andl %rbx, %rax
	addl $1, %rax
	jmp L1_return
	andl %rbx, %rax
	jmp L1_fakereturn
L1_return:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
