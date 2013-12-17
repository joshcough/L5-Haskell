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
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_L_1
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_L_1:
	movl %esi, %ebx
	movl %edi, %edi
	pushl $0
	pushl $15
	call allocate
	addl $8, %esp
	movl $1, 4(%eax)
	movl $3, 8(%eax)
	movl $5, 12(%eax)
	movl $7, 16(%eax)
	movl $9, 20(%eax)
	movl $11, 24(%eax)
	movl $13, 28(%eax)
	movl %eax, %esi
	movl $7, %eax
	movl 0(%esi), %edx
	sall $1, %edx
	addl $1, %edx
	cmpl %edx, %eax
	jl L1_oka_5
	jmp L1_die_4
L1_oka_5:
	cmpl $0, %eax
	jge L1_okb_6
	jmp L1_die_4
L1_die_4:
	pushl $7
	pushl %esi
	call print_error
	addl $8, %esp
L1_okb_6:
	sarl $1, %eax
	imull $4, %eax
	addl %esi, %eax
	movl $643, 4(%eax)
	movl $1, %eax
	movl $7, %eax
	movl 0(%esi), %edx
	sall $1, %edx
	addl $1, %edx
	cmpl %edx, %eax
	jl L1_oka_10
	jmp L1_die_9
L1_oka_10:
	cmpl $0, %eax
	jge L1_okb_11
	jmp L1_die_9
L1_die_9:
	pushl $7
	pushl %esi
	call print_error
	addl $8, %esp
L1_okb_11:
	sarl $1, %eax
	imull $4, %eax
	addl %esi, %eax
	movl 4(%eax), %esi
	pushl %esi
	call print
	addl $4, %esp
	movl %eax, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
