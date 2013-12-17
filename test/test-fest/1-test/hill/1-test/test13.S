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
	movl %esp, %eax
	subl $32, %esp
	movl $17, 0(%esp)
	movl $19, 4(%esp)
	movl $21, 8(%esp)
	movl $33, 12(%esp)
	movl $0, %esi
L1_loop_top:
	addl $1, %esi
	movl 0(%esp), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 4(%esp), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 8(%esp), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 12(%esp), %eax
	pushl %eax
	call print
	addl $4, %esp
	cmpl $3, %esi
	jl L1_loop_top
	jmp L1_loop_fin
L1_loop_fin:
	addl $32, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
