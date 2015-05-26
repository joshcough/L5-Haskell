	.file	"./test/test-fest/L5-tests-from-2010/burgener/test/11.L5"
	.text
.globl _go
_go:
_L1_main:
	subq $8, %rsp
	call _L1__L3main_
	addq $8, %rsp
	ret
	addq $8, %rsp
	ret
_L1__L3main_:
	subq $168, %rsp
	movq $3, %rdi
	movq $1, %rsi
	call _allocate
	movq $1, 8(%rax)
	movq %rax, 144(%rsp)
	movq $3, %rdi
	movq $1, %rsi
	call _allocate
	movq 144(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq %rax, 112(%rsp)
	movq _L1_l5lbl_0@GOTPCREL(%rip), %r15
	movq %r15, 96(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 96(%rsp), %rsi
	movq %rsi, 8(%rax)
	movq 112(%rsp), %rsi
	movq %rsi, 16(%rax)
	movq %rax, 128(%rsp)
	movq $1, 48(%rsp)
	movq 48(%rsp), %rax
	sarq $1, %rax
	movq %rax, 48(%rsp)
	movq 144(%rsp), %rax
	movq 0(%rax), %rax
	movq %rax, 32(%rsp)
	movq 32(%rsp), %rax
	movq 48(%rsp), %rsi
	cmpq %rsi, %rax
	jle _L1__l3_2
	jmp _L1__l3_4
_L1__l3_2:
	movq 48(%rsp), %rax
	salq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	addq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rbx
	movq 144(%rsp), %rax
	movq %rax, %rdi
	movq %rbx, %rsi
	call _print_error
_L1__l3_4:
	movq 48(%rsp), %rax
	cmpq $0, %rax
	jl _L1__l3_2
	jmp _L1__l3_3
_L1__l3_3:
	movq 48(%rsp), %rax
	addq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	imulq $8, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	movq 144(%rsp), %rsi
	addq %rsi, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	movq 128(%rsp), %rsi
	movq %rsi, 0(%rax)
	movq $1, 48(%rsp)
	movq $1, 80(%rsp)
	movq 80(%rsp), %rax
	sarq $1, %rax
	movq %rax, 80(%rsp)
	movq 144(%rsp), %rax
	movq 0(%rax), %rax
	movq %rax, 16(%rsp)
	movq 16(%rsp), %rsi
	movq 80(%rsp), %rax
	cmpq %rax, %rsi
	jle _L1__l3_6
	jmp _L1__l3_8
_L1__l3_6:
	movq 80(%rsp), %rax
	salq $1, %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	addq $1, %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	movq 144(%rsp), %rbx
	movq %rbx, %rdi
	movq %rax, %rsi
	call _print_error
_L1__l3_8:
	movq 80(%rsp), %rax
	cmpq $0, %rax
	jl _L1__l3_6
	jmp _L1__l3_7
_L1__l3_7:
	movq 80(%rsp), %rax
	addq $1, %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	imulq $8, %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	movq 144(%rsp), %rsi
	addq %rsi, %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	movq 0(%rax), %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rax
	movq 8(%rax), %rax
	movq %rax, 64(%rsp)
	movq 80(%rsp), %rax
	movq 16(%rax), %rax
	movq %rax, 0(%rsp)
	movq 0(%rsp), %rdi
	movq $21, %rsi
	movq 64(%rsp), %rax
	addq $168, %rsp
	jmp *%rax
_L1_l5lbl_0:
	subq $152, %rsp
	movq %rdi, %rdi
	movq %rsi, %r10
	movq $1, %rax
	sarq $1, %rax
	movq 0(%rdi), %rsi
	movq %rsi, 64(%rsp)
	movq 64(%rsp), %rsi
	cmpq %rax, %rsi
	jle _L1__l3_10
	jmp _L1__l3_12
_L1__l3_10:
	salq $1, %rax
	addq $1, %rax
	movq %rdi, %rdi
	movq %rax, %rsi
	call _print_error
_L1__l3_12:
	cmpq $0, %rax
	jl _L1__l3_10
	jmp _L1__l3_11
_L1__l3_11:
	addq $1, %rax
	imulq $8, %rax
	addq %rdi, %rax
	movq 0(%rax), %rax
	movq %r10, 0(%rsp)
	cmpq $1, %r10
	je _L1__l3_15
	jmp _L1__l3_14
_L1__l3_14:
	movq $1, %rsi
	sarq $1, %rsi
	movq 0(%rax), %rdi
	movq %rdi, 48(%rsp)
	movq 48(%rsp), %rdi
	cmpq %rsi, %rdi
	jle _L1__l3_17
	jmp _L1__l3_19
_L1__l3_17:
	salq $1, %rsi
	addq $1, %rsi
	movq %rax, %rdi
	movq %rsi, %rsi
	call _print_error
_L1__l3_19:
	cmpq $0, %rsi
	jl _L1__l3_17
	jmp _L1__l3_18
_L1__l3_18:
	addq $1, %rsi
	imulq $8, %rsi
	addq %rax, %rsi
	movq 0(%rsi), %rsi
	movq 8(%rsi), %rax
	movq 16(%rsi), %rsi
	movq %rsi, 80(%rsp)
	movq %r10, 96(%rsp)
	movq 96(%rsp), %rsi
	subq $3, %rsi
	movq %rsi, 96(%rsp)
	movq 96(%rsp), %rsi
	addq $1, %rsi
	movq %rsi, 96(%rsp)
	movq 80(%rsp), %rdi
	movq 96(%rsp), %rsi
	call *%rax
	movq %rax, 112(%rsp)
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, 16(%rsp)
	movq 112(%rsp), %rdi
	movq 16(%rsp), %rsi
	addq $152, %rsp
	jmp _L1_l4__11
_L1__l3_15:
	movq %r10, %rdi
	call _print
	movq %rax, 128(%rsp)
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, 32(%rsp)
	movq 128(%rsp), %rdi
	movq 32(%rsp), %rsi
	addq $152, %rsp
	jmp _L1_l4__11
	addq $152, %rsp
	ret
_L1_l4__11:
	subq $8, %rsp
	movq %rdi, %rax
	movq %rsi, %rcx
	movq %rax, %rax
	addq $8, %rsp
	ret
	addq $8, %rsp
	ret
	
