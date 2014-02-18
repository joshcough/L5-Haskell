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
	movl $2, %rsi
L1_w:
	jmp L1_wefowifwef_wefowfoiwejf_AOIOAOIDJOAI_93493sdfij23nfsdUH
L1_wefowifwef_wefowfoiwejf_AOIOAOIDJOAI_93493sdfij23nfsdUH:
	jmp L1_w112345
L1_w112345:
	jmp L1_x9
L1_x9:
	jmp L1__
L1__:
	jmp L1___________________________________
L1___________________________________:
	jmp L1________________999
L1________________999:
	jmp L1________________aaa
L1________________aaa:
	jmp L1________________AAA
L1________________AAA:
	subl $1, %rsi
	cmpl $0, %rsi
	je L1__A_A_A_A_A_A_A_A_
	jmp L1_w
L1__A_A_A_A_A_A_A_A_:
	movl $3, %rax
	pushl %rax
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
