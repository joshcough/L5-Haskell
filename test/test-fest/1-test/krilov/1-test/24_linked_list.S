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
	pushl $3
	pushl $5
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl $5, %ebx
	movl %esi, %edi
L1_allocate_next:
	pushl %ebx
	pushl $5
	call allocate
	addl $8, %esp
	movl 4(%eax), %ebx
	movl %eax, 8(%edi)
	movl %eax, %edi
	addl $2, %ebx
	cmpl $11, %ebx
	jle L1_allocate_next
	jmp L1_done_allocating
L1_done_allocating:
	pushl $9999
	pushl $5
	call allocate
	addl $8, %esp
	movl %eax, 8(%edi)
	movl %esi, %edi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_print_list
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_print_list:
	movl 4(%edi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl 8(%edi), %edi
	movl 4(%edi), %eax
	cmpl $9999, %eax
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
