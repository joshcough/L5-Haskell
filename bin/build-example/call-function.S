	.file	"fun1.c"
	.text
.globl _go
_go:
	subq $8, %rsp
	call L1_fun
	movq $7, %rdi
	call _print
	addq $8, %rsp
	ret
