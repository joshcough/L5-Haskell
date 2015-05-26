	.file	"2.L1"
	.text
.globl _go
_go:
_L1_main:
	subq $8, %rsp
_L1_main:
	call _L1___L3main__
	addq $8, %rsp
	ret
_L1___L3main__:
	subq $8, %rsp
	movq $3, %rdi
	movq $5, %rsi
	movq $7, %rdx
	call _L1_f1
	movq %rax, %rax
	movq %rax, %rdi
	call _print
	movq %rax, %rax
	addq $8, %rsp
	ret
_L1_f1:
	subq $8, %rsp
	movq %rdi, %rax
	movq %rsi, 8(%rsp)
	movq %rdx, 0(%rsp)
	movq %rax, %rax
	cmpq $3, %rax
	je _L1__tempL3_1
	jmp _L1__tempL3_2
_L1__tempL3_1:
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, %rax
	movq 8(%rsp), %rdi
	movq %rax, %rsi
	addq $8, %rsp
	jmp _L1_idf
_L1__tempL3_2:
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, %rax
	movq 0(%rsp), %rdi
	movq %rax, %rsi
	addq $8, %rsp
	jmp _L1_idf
	addq $8, %rsp
	ret
_L1_idf:
	subq $8, %rsp
	movq %rdi, %rcx
	movq %rsi, %rax
	movq %rcx, %rax
	addq $8, %rsp
	ret
	addq $8, %rsp
	ret
	
