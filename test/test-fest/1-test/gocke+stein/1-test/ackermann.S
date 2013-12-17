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
	movl $0, %eax
	movl $0, %edi
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_0:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_decode
L1_Generated_Label_1:
	pushl %eax
	call print
	addl $4, %esp
	movl $3, %eax
	movl $0, %edi
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_2:
	pushl $L1_Generated_Label_3
	pushl %ebp
	movl %esp, %ebp
	jmp L1_decode
L1_Generated_Label_3:
	pushl %eax
	call print
	addl $4, %esp
	movl $1, %eax
	movl $3, %edi
	pushl $L1_Generated_Label_4
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_4:
	pushl $L1_Generated_Label_5
	pushl %ebp
	movl %esp, %ebp
	jmp L1_decode
L1_Generated_Label_5:
	pushl %eax
	call print
	addl $4, %esp
	movl $3, %eax
	movl $3, %edi
	pushl $L1_Generated_Label_6
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_6:
	pushl $L1_Generated_Label_7
	pushl %ebp
	movl %esp, %ebp
	jmp L1_decode
L1_Generated_Label_7:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_ack:
	cmpl $0, %eax
	je L1_ret
	jmp L1_cont
L1_ret:
	movl %edi, %eax
	addl $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_cont:
	cmpl $0, %edi
	je L1_ack1
	jmp L1_ackfull
L1_ack1:
	subl $1, %eax
	movl $1, %edi
	movl %ebp, %esp
	jmp L1_ack
L1_ackfull:
	movl %eax, %esi
	subl $1, %edi
	pushl $L1_Generated_Label_8
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_8:
	movl %eax, %edi
	movl %esi, %eax
	subl $1, %eax
	movl %ebp, %esp
	jmp L1_ack
L1_decode:
	imull $2, %eax
	addl $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
