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
L1_print1:
	pushl $3
	call print
	addl $4, %esp
	jmp L1_print3
L1_print2:
	pushl $5
	call print
	addl $4, %esp
	movl $3, %rax
	movl $5, %rbx
	cmpl %rbx, %rax
	jl L1_print4
	jmp L1_bad
L1_print3:
	pushl $7
	call print
	addl $4, %esp
	jmp L1_print2
L1_print4:
	pushl $9
	call print
	addl $4, %esp
	movl $5, %rax
	movl $3, %rbx
	cmpl %rbx, %rax
	jl L1_bad
	jmp L1_print5
L1_print5:
	pushl $11
	call print
	addl $4, %esp
	jmp L1_done
L1_bad:
	pushl $999999
	call print
	addl $4, %esp
L1_done:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
