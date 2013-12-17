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
	pushl $23
	pushl $21
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $201, %eax
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
	pushl $201
	pushl %esi
	call print_error
	addl $8, %esp
L1_okb_6:
	sarl $1, %eax
	imull $4, %eax
	addl %esi, %eax
	movl $643, 4(%eax)
	movl $1, %eax
	movl %ebx, %esi
	movl %edi, %edi
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
