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
	movl $5, %edi
	movl $15, %esi
	movl $25, %ebx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_helper
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_helper:
	pushl %edi
	call print
	addl $4, %esp
	pushl %esi
	call print
	addl $4, %esp
	cmpl %esi, %edi
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl %ebp, %esp
	popl %ebp
	ret
L1_done:
	pushl %ebx
	call print
	addl $4, %esp
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
