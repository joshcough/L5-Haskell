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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L_1
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_L_1:
	movl %rsi, %rbx
	movl %rdi, %rdi
	pushl $23
	pushl $21
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $201, %rax
	movl 0(%rsi), %rdx
	sall $1, %rdx
	addl $1, %rdx
	cmpl %rdx, %rax
	jl L1_oka_5
	jmp L1_die_4
L1_oka_5:
	cmpl $0, %rax
	jge L1_okb_6
	jmp L1_die_4
L1_die_4:
	pushl $201
	pushl %rsi
	call print_error
	addl $8, %esp
L1_okb_6:
	sarl $1, %rax
	imull $4, %rax
	addl %rsi, %rax
	movl $643, 4(%rax)
	movl $1, %rax
	movl %rbx, %rsi
	movl %rdi, %rdi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
