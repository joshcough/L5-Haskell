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
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, %rsi
	movl %rax, %rdi
	pushl $7777
	pushl $9
	call allocate
	addl $8, %esp
	movl %rax, 16(%rsi)
	pushl $5
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, %rdi
	pushl $7
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, %rdi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl 16(%rdi), %rdi
	pushl $9
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 12(%rdi)
	movl %rax, %rdi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl %rsi, %rdi
	pushl $11
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 12(%rdi)
	movl %rax, %rdi
	pushl $13
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, %rdi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl 16(%rdi), %rdi
	pushl $15
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 12(%rdi)
	movl %rax, %rdi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %rdi, 16(%rax)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl %rax, 8(%rdi)
	movl %rax, 12(%rdi)
	movl %rsi, %rdi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_0:
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_traverse_tree:
	movl 4(%rdi), %rax
	pushl %rax
	call print
	addl $4, %esp
	movl %rdi, %rsi
	movl 8(%rdi), %rdi
	movl 4(%rdi), %rax
	cmpl $9999, %rax
	je L1_skip_left_child
	jmp L1_visit_left_child
L1_visit_left_child:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_1:
L1_skip_left_child:
	movl %rsi, %rdi
	movl 12(%rdi), %rdi
	movl 4(%rdi), %rax
	cmpl $9999, %rax
	je L1_skip_right_child
	jmp L1_visit_right_child
L1_visit_right_child:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_2:
L1_skip_right_child:
	movl %rsi, %rdi
	movl 16(%rdi), %rsi
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
