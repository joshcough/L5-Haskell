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
L1_print1:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_print3
L1_print2:
	pushl $5
	call print
	addl $4, %esp
	jmp L1_print5
L1_print3:
	pushl $7
	call print
	addl $4, %esp
	jmp L1_print2
L1_print4:
	pushl $9
	call print
	addl $4, %esp
	jmp L1_done
L1_print5:
	pushl $11
	call print
	addl $4, %esp
	jmp L1_print4
L1_done:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
