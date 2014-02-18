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
	pushl $1
	call print
	addl $4, %esp
	pushl $3
	call print
	addl $4, %esp
	pushl $5
	call print
	addl $4, %esp
	pushl $7
	call print
	addl $4, %esp
	pushl $9
	call print
	addl $4, %esp
	pushl $11
	call print
	addl $4, %esp
	pushl $13
	call print
	addl $4, %esp
	pushl $15
	call print
	addl $4, %esp
	pushl $17
	call print
	addl $4, %esp
	pushl $19
	call print
	addl $4, %esp
	pushl $21
	call print
	addl $4, %esp
	pushl $23
	call print
	addl $4, %esp
	pushl $25
	call print
	addl $4, %esp
	pushl $27
	call print
	addl $4, %esp
	pushl $29
	call print
	addl $4, %esp
	pushl $31
	call print
	addl $4, %esp
	pushl $33
	call print
	addl $4, %esp
	pushl $35
	call print
	addl $4, %esp
	pushl $37
	call print
	addl $4, %esp
	pushl $39
	call print
	addl $4, %esp
	pushl $41
	call print
	addl $4, %esp
	pushl $43
	call print
	addl $4, %esp
	pushl $45
	call print
	addl $4, %esp
	pushl $47
	call print
	addl $4, %esp
	pushl $49
	call print
	addl $4, %esp
	pushl $51
	call print
	addl $4, %esp
	pushl $53
	call print
	addl $4, %esp
	pushl $55
	call print
	addl $4, %esp
	pushl $57
	call print
	addl $4, %esp
	pushl $59
	call print
	addl $4, %esp
	pushl $61
	call print
	addl $4, %esp
	pushl $63
	call print
	addl $4, %esp
	pushl $65
	call print
	addl $4, %esp
	pushl $67
	call print
	addl $4, %esp
	pushl $69
	call print
	addl $4, %esp
	pushl $71
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
