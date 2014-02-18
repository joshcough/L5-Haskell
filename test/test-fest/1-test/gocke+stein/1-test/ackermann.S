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
	movl $0, %rax
	movl $0, %rdi
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
	pushl %rax
	call print
	addl $4, %esp
	movl $3, %rax
	movl $0, %rdi
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
	pushl %rax
	call print
	addl $4, %esp
	movl $1, %rax
	movl $3, %rdi
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
	pushl %rax
	call print
	addl $4, %esp
	movl $3, %rax
	movl $3, %rdi
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
	pushl %rax
	call print
	addl $4, %esp
	popl %ebp
	popl %rdi
	popl %rsi
	popl %rbx
	leave
	ret
L1_ack:
	cmpl $0, %rax
	je L1_ret
	jmp L1_cont
L1_ret:
	movl %rdi, %rax
	addl $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
L1_cont:
	cmpl $0, %rdi
	je L1_ack1
	jmp L1_ackfull
L1_ack1:
	subl $1, %rax
	movl $1, %rdi
	movl %ebp, %esp
	jmp L1_ack
L1_ackfull:
	movl %rax, %rsi
	subl $1, %rdi
	pushl $L1_Generated_Label_8
	pushl %ebp
	movl %esp, %ebp
	jmp L1_ack
L1_Generated_Label_8:
	movl %rax, %rdi
	movl %rsi, %rax
	subl $1, %rax
	movl %ebp, %esp
	jmp L1_ack
L1_decode:
	imull $2, %rax
	addl $1, %rax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
