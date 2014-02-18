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
	pushl $3
	pushl $5
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl $5, %rbx
	movl %rsi, %rdi
L1_allocate_next:
	pushl %rbx
	pushl $5
	call allocate
	addl $8, %esp
	movl 4(%rax), %rbx
	movl %rax, 8(%rdi)
	movl %rax, %rdi
	addl $2, %rbx
	cmpl $11, %rbx
	jle L1_allocate_next
	jmp L1_done_allocating
L1_done_allocating:
	pushl $9999
	pushl $5
	call allocate
	addl $8, %esp
	movl %rax, 8(%rdi)
	movl %rsi, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_print_list
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_print_list:
	movl 4(%rdi), %rax
	pushl %rax
	call print
	addl $4, %esp
	movl 8(%rdi), %rdi
	movl 4(%rdi), %rax
	cmpl $9999, %rax
	je L1_done
	jmp L1_print_next
L1_print_next:
	movl %ebp, %esp
	jmp L1_print_list
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
