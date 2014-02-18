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
L1_START:
	movl $9, %rax
	movl $3, %rcx
	jmp L1_PART2
L1_PART1:
	imull %rcx, %rax
	jmp L1_END
L1_PART2:
	subl %rcx, %rax
	jmp L1_PART1
L1_END:
	sall $1, %rax
	addl $1, %rax
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
