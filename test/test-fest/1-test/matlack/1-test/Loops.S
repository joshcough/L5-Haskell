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
	movl $85, %eax
	movl $1, %ebx
L1_loop:
	cmpl $0, %eax
	je L1_done
	jmp L1_keep_going
L1_keep_going:
	subl $1, %eax
	addl $3, %ebx
	jmp L1_loop
L1_done:
	movl %ebx, %ecx
L1_loop2:
	cmpl $42, %eax
	jl L1_done2
	jmp L1_keep_going2
L1_keep_going2:
	imull %ebx, %ecx
	subl $1, %ebx
	jmp L1_loop2
L1_done2:
	imull $2, %ecx
	addl $1, %ecx
	pushl %ecx
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
