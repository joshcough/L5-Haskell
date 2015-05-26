	.file	"./test/test-fest/L5-tests-from-2010/mcglynn/test/10.L5"
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
	subq $424, %rsp
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, 224(%rsp)
	movq _L1_l5lbl_2@GOTPCREL(%rip), %r15
	movq %r15, 208(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 208(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq 224(%rsp), %rcx
	movq %rcx, 16(%rax)
	movq %rax, 352(%rsp)
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, 288(%rsp)
	movq _L1_l5lbl_3@GOTPCREL(%rip), %r15
	movq %r15, 272(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 272(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq 288(%rsp), %rcx
	movq %rcx, 16(%rax)
	movq %rax, 400(%rsp)
	movq $1, %rdi
	movq $1, %rsi
	call _allocate
	movq %rax, 336(%rsp)
	movq _L1_l5lbl_4@GOTPCREL(%rip), %r15
	movq %r15, 320(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 320(%rsp), %rsi
	movq %rsi, 8(%rax)
	movq 336(%rsp), %rsi
	movq %rsi, 16(%rax)
	movq %rax, 368(%rsp)
	movq 352(%rsp), %rax
	movq %rax, 144(%rsp)
	movq 144(%rsp), %rax
	movq 8(%rax), %rax
	movq %rax, 240(%rsp)
	movq 144(%rsp), %rax
	movq 16(%rax), %rax
	movq %rax, 128(%rsp)
	movq 128(%rsp), %rdi
	movq $11, %rsi
	movq 240(%rsp), %rax
	call *%rax
	movq %rax, 384(%rsp)
	movq 400(%rsp), %rax
	movq %rax, 176(%rsp)
	movq 176(%rsp), %rax
	movq 8(%rax), %rax
	movq %rax, 304(%rsp)
	movq 176(%rsp), %rax
	movq 16(%rax), %rax
	movq %rax, 160(%rsp)
	movq 160(%rsp), %rdi
	movq 384(%rsp), %rsi
	movq 304(%rsp), %rax
	call *%rax
	movq %rax, 16(%rsp)
	movq 16(%rsp), %rax
	movq %rax, %rdi
	call _print
	movq %rax, 32(%rsp)
	movq 368(%rsp), %rax
	movq %rax, 96(%rsp)
	movq 96(%rsp), %rax
	movq 8(%rax), %rax
	movq %rax, 256(%rsp)
	movq 96(%rsp), %rax
	movq 16(%rax), %rax
	movq %rax, 64(%rsp)
	movq 64(%rsp), %rdi
	movq 384(%rsp), %rsi
	movq $13, %rdx
	movq 256(%rsp), %rax
	call *%rax
	movq %rax, 48(%rsp)
	movq 400(%rsp), %rax
	movq %rax, 112(%rsp)
	movq 112(%rsp), %rax
	movq 8(%rax), %rax
	movq %rax, 192(%rsp)
	movq 112(%rsp), %rax
	movq 16(%rax), %rax
	movq %rax, 80(%rsp)
	movq 80(%rsp), %rdi
	movq 384(%rsp), %rsi
	movq 192(%rsp), %rax
	call *%rax
	movq %rax, 0(%rsp)
	movq 0(%rsp), %rax
	movq %rax, %rdi
	call _print
	movq %rax, %rax
	addq $424, %rsp
	ret
_L1_l5lbl_2:
	subq $216, %rsp
	movq %rdi, 0(%rsp)
	movq %rsi, 176(%rsp)
	movq $3, %rdi
	movq $1, %rsi
	call _allocate
	movq $1, 8(%rax)
	movq %rax, 192(%rsp)
	movq $3, %rdi
	movq $1, %rsi
	call _allocate
	movq 192(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq %rax, 96(%rsp)
	movq _L1_l5lbl_0@GOTPCREL(%rip), %r15
	movq %r15, 80(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 80(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq 96(%rsp), %rcx
	movq %rcx, 16(%rax)
	movq %rax, 144(%rsp)
	movq $3, %rdi
	movq $1, %rsi
	call _allocate
	movq 192(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq %rax, 160(%rsp)
	movq _L1_l5lbl_1@GOTPCREL(%rip), %r15
	movq %r15, 128(%rsp)
	movq $5, %rdi
	movq $1, %rsi
	call _allocate
	movq 128(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq 160(%rsp), %rcx
	movq %rcx, 16(%rax)
	movq %rax, 112(%rsp)
	movq $7, %rdi
	movq $1, %rsi
	call _allocate
	movq 176(%rsp), %rcx
	movq %rcx, 8(%rax)
	movq 144(%rsp), %rcx
	movq %rcx, 16(%rax)
	movq 112(%rsp), %rcx
	movq %rcx, 24(%rax)
	movq %rax, 64(%rsp)
	movq $1, 48(%rsp)
	movq 48(%rsp), %rax
	sarq $1, %rax
	movq %rax, 48(%rsp)
	movq 192(%rsp), %rax
	movq 0(%rax), %rax
	movq %rax, 32(%rsp)
	movq 32(%rsp), %rax
	movq 48(%rsp), %rcx
	cmpq %rcx, %rax
	jle _L1__l3_6
	jmp _L1__l3_8
_L1__l3_6:
	movq 48(%rsp), %rax
	salq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	addq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rbx
	movq 192(%rsp), %rax
	movq %rax, %rdi
	movq %rbx, %rsi
	call _print_error
_L1__l3_8:
	movq 48(%rsp), %rax
	cmpq $0, %rax
	jl _L1__l3_6
	jmp _L1__l3_7
_L1__l3_7:
	movq 48(%rsp), %rax
	addq $1, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	imulq $8, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	movq 192(%rsp), %rcx
	addq %rcx, %rax
	movq %rax, 48(%rsp)
	movq 48(%rsp), %rax
	movq 64(%rsp), %rcx
	movq %rcx, 0(%rax)
	movq $1, 48(%rsp)
	movq $1, %rax
	sarq $1, %rax
	movq 192(%rsp), %rcx
	movq 0(%rcx), %rcx
	movq %rcx, 16(%rsp)
	movq 16(%rsp), %rcx
	cmpq %rax, %rcx
	jle _L1__l3_10
	jmp _L1__l3_12
_L1__l3_10:
	salq $1, %rax
	addq $1, %rax
	movq 192(%rsp), %rbx
	movq %rbx, %rdi
	movq %rax, %rsi
	call _print_error
_L1__l3_12:
	cmpq $0, %rax
	jl _L1__l3_10
	jmp _L1__l3_11
_L1__l3_11:
	addq $1, %rax
	imulq $8, %rax
	movq 192(%rsp), %rcx
	addq %rcx, %rax
	movq 0(%rax), %rax
	addq $216, %rsp
	ret
	addq $216, %rsp
	ret
_L1_l5lbl_0:
	subq $8, %rsp
	movq %rdi, %rcx
	movq $1, %rsi
	sarq $1, %rsi
	movq 0(%rcx), %rax
	cmpq %rsi, %rax
	jle _L1__l3_14
	jmp _L1__l3_16
_L1__l3_14:
	salq $1, %rsi
	addq $1, %rsi
	movq %rcx, %rdi
	movq %rsi, %rsi
	call _print_error
_L1__l3_16:
	cmpq $0, %rsi
	jl _L1__l3_14
	jmp _L1__l3_15
_L1__l3_15:
	addq $1, %rsi
	imulq $8, %rsi
	addq %rcx, %rsi
	movq 0(%rsi), %rsi
	movq $1, %rdx
	sarq $1, %rdx
	movq 0(%rsi), %rax
	cmpq %rdx, %rax
	jle _L1__l3_18
	jmp _L1__l3_20
_L1__l3_18:
	salq $1, %rdx
	addq $1, %rdx
	movq %rsi, %rdi
	movq %rdx, %rsi
	call _print_error
_L1__l3_20:
	cmpq $0, %rdx
	jl _L1__l3_18
	jmp _L1__l3_19
_L1__l3_19:
	addq $1, %rdx
	imulq $8, %rdx
	addq %rsi, %rdx
	movq 0(%rdx), %rdx
	movq $1, %rax
	sarq $1, %rax
	movq 0(%rdx), %rcx
	cmpq %rax, %rcx
	jle _L1__l3_22
	jmp _L1__l3_24
_L1__l3_22:
	salq $1, %rax
	addq $1, %rax
	movq %rdx, %rdi
	movq %rax, %rsi
	call _print_error
_L1__l3_24:
	cmpq $0, %rax
	jl _L1__l3_22
	jmp _L1__l3_23
_L1__l3_23:
	addq $1, %rax
	imulq $8, %rax
	addq %rdx, %rax
	movq 0(%rax), %rax
	addq $8, %rsp
	ret
	addq $8, %rsp
	ret
_L1_l5lbl_1:
	subq $8, %rsp
	movq %rdi, %rcx
	movq %rsi, %rsi
	movq $1, %rdi
	sarq $1, %rdi
	movq 0(%rcx), %rax
	cmpq %rdi, %rax
	jle _L1__l3_26
	jmp _L1__l3_28
_L1__l3_26:
	salq $1, %rdi
	addq $1, %rdi
	movq %rcx, %rdi
	movq %rdi, %rsi
	call _print_error
_L1__l3_28:
	cmpq $0, %rdi
	jl _L1__l3_26
	jmp _L1__l3_27
_L1__l3_27:
	addq $1, %rdi
	imulq $8, %rdi
	addq %rcx, %rdi
	movq 0(%rdi), %rdi
	movq $1, %rdx
	sarq $1, %rdx
	movq 0(%rdi), %rax
	cmpq %rdx, %rax
	jle _L1__l3_30
	jmp _L1__l3_32
_L1__l3_30:
	salq $1, %rdx
	addq $1, %rdx
	movq %rdi, %rdi
	movq %rdx, %rsi
	call _print_error
_L1__l3_32:
	cmpq $0, %rdx
	jl _L1__l3_30
	jmp _L1__l3_31
_L1__l3_31:
	addq $1, %rdx
	imulq $8, %rdx
	addq %rdi, %rdx
	movq 0(%rdx), %rdx
	movq $1, %rax
	sarq $1, %rax
	movq 0(%rdx), %rcx
	cmpq %rax, %rcx
	jle _L1__l3_34
	jmp _L1__l3_36
_L1__l3_34:
	salq $1, %rax
	addq $1, %rax
	movq %rdx, %rdi
	movq %rax, %rsi
	call _print_error
_L1__l3_36:
	cmpq $0, %rax
	jl _L1__l3_34
	jmp _L1__l3_35
_L1__l3_35:
	addq $1, %rax
	imulq $8, %rax
	addq %rdx, %rax
	movq %rsi, 0(%rax)
	movq $1, %rax
	addq $8, %rsp
	ret
	addq $8, %rsp
	ret
_L1_l5lbl_3:
	subq $8, %rsp
	movq %rdi, %rax
	movq %rsi, %r10
	movq $3, %rdi
	sarq $1, %rdi
	movq 0(%r10), %rax
	cmpq %rdi, %rax
	jle _L1__l3_38
	jmp _L1__l3_40
_L1__l3_38:
	salq $1, %rdi
	addq $1, %rdi
	movq %r10, %rdi
	movq %rdi, %rsi
	call _print_error
_L1__l3_40:
	cmpq $0, %rdi
	jl _L1__l3_38
	jmp _L1__l3_39
_L1__l3_39:
	addq $1, %rdi
	imulq $8, %rdi
	addq %r10, %rdi
	movq 0(%rdi), %rdi
	movq 8(%rdi), %rax
	movq 16(%rdi), %rdi
	movq %rdi, %rdi
	addq $8, %rsp
	jmp *%rax
	addq $8, %rsp
	ret
_L1_l5lbl_4:
	subq $8, %rsp
	movq %rdi, %rax
	movq %rsi, %rdi
	movq %rdx, %r10
	movq $5, %rsi
	sarq $1, %rsi
	movq 0(%rdi), %rax
	cmpq %rsi, %rax
	jle _L1__l3_42
	jmp _L1__l3_44
_L1__l3_42:
	salq $1, %rsi
	addq $1, %rsi
	movq %rdi, %rdi
	movq %rsi, %rsi
	call _print_error
_L1__l3_44:
	cmpq $0, %rsi
	jl _L1__l3_42
	jmp _L1__l3_43
_L1__l3_43:
	addq $1, %rsi
	imulq $8, %rsi
	addq %rdi, %rsi
	movq 0(%rsi), %rsi
	movq 8(%rsi), %rax
	movq 16(%rsi), %rsi
	movq %rsi, %rdi
	movq %r10, %rsi
	addq $8, %rsp
	jmp *%rax
	addq $8, %rsp
	ret
	
