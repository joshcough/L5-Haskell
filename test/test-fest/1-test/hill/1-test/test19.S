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
	jmp L1_good1
L1_good1:
	jmp L1_good2
L1_good2:
	jmp L1_good3
L1_good3:
	pushl $41
	call print
	addl $4, %esp
	jmp L1_end
L1_bad:
	pushl $21
	call print
	addl $4, %esp
L1_end:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
