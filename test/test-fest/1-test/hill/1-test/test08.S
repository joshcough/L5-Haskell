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
L1_START:
	movl $9, %eax
	movl $3, %ecx
	jmp L1_PART2
L1_PART1:
	imull %ecx, %eax
	jmp L1_END
L1_PART2:
	subl %ecx, %eax
	jmp L1_PART1
L1_END:
	sall $1, %eax
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
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
