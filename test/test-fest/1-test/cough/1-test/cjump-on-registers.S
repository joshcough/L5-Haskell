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
	movl $6, %rcx
	movl $5, %rdx
	cmpl %rdx, %rcx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	pushl $9999999
	call print
	addl $4, %esp
L1_done:
	movl %rcx, %rsi
	movl %rdx, %rdi
	pushl %rdx
	call print
	addl $4, %esp
	cmp %rsi, %rdi
	setl %bl
	movzbl %bl, %rbx
	pushl %rbx
	call print
	addl $4, %esp
	cmp %rsi, %rdi
	setle %bl
	movzbl %bl, %rbx
	pushl %rbx
	call print
	addl $4, %esp
	cmp %rdi, %rsi
	setl %bl
	movzbl %bl, %rbx
	cmpl $0, %rbx
	jle L1_terminate
	jmp L1_aint_gonna_happen
L1_aint_gonna_happen:
L1_terminate:
	pushl $85
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
