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
	movl $1, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printone
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_printone:
	movl %eax, %esi
	sall $1, %esi
	addl $1, %esi
	pushl %esi
	call print
	addl $4, %esp
	movl %esi, %eax
	sarl $1, %eax
	addl $1, %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_skipone
L1_Generated_Label_1:
	movl %ebp, %esp
	popl %ebp
	ret
L1_skipone:
	cmpl $10, %eax
	jl L1_go_on
	jmp L1_quit
L1_go_on:
	addl $1, %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_printone
L1_Generated_Label_2:
L1_quit:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
