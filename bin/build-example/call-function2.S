	.file	"fun2.c"
	.text
L1_fun:
	subq $8, %rsp
	movq $5, %rdi
	call _print
	addq $8, %rsp
	ret
	
