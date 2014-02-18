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
	jmp L1_next1
L1_next1:
	jmp L1_next2
L1_next2:
	jmp L1_next3
L1_next3:
	jmp L1_next4
L1_next4:
	jmp L1_next5
L1_next5:
	jmp L1_next6
L1_next6:
	jmp L1_next7
L1_next7:
	jmp L1_next8
L1_next8:
	jmp L1_next9
L1_next9:
	jmp L1_next10
L1_next10:
	jmp L1_next11
L1_next11:
	jmp L1_next12
L1_next12:
	jmp L1_next13
L1_next13:
	movl $-1, %rdx
	pushl %rdx
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
