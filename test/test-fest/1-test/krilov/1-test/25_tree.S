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
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, %esi
	movl %eax, %edi
	pushl $7777
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, 16(%esi)
	pushl $5
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, %edi
	pushl $7
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, %edi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl 16(%edi), %edi
	pushl $9
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 12(%edi)
	movl %eax, %edi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl %esi, %edi
	pushl $11
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 12(%edi)
	movl %eax, %edi
	pushl $13
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, %edi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl 16(%edi), %edi
	pushl $15
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 12(%edi)
	movl %eax, %edi
	pushl $9999
	pushl $9
	call allocate
	addl $8, %esp
	movl %edi, 16(%eax)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl %eax, 8(%edi)
	movl %eax, 12(%edi)
	movl %esi, %edi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_traverse_tree:
	movl 4(%edi), %eax
	pushl %eax
	call print
	addl $4, %esp
	movl %edi, %esi
	movl 8(%edi), %edi
	movl 4(%edi), %eax
	cmpl $9999, %eax
	je L1_skip_left_child
	jmp L1_visit_left_child
L1_visit_left_child:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_1:
L1_skip_left_child:
	movl %esi, %edi
	movl 12(%edi), %edi
	movl 4(%edi), %eax
	cmpl $9999, %eax
	je L1_skip_right_child
	jmp L1_visit_right_child
L1_visit_right_child:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_traverse_tree
L1_Generated_Label_2:
L1_skip_right_child:
	movl %esi, %edi
	movl 16(%edi), %esi
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
