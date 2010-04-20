	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebx
	pushl %esi
	pushl %edi
	pushl %ebp
	movl	%esp, %ebp
	movl $6, %ecx
	movl $5, %edx
	cmpl %edx, %ecx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	pushl $9999999
	call print
	addl $4,%esp
L1_done:
	movl %ecx, %esi
	movl %edx, %edi
	pushl %edx
	call print
	addl $4,%esp
	cmp %esi, %edi
	setl %bl
	movzbl %bl, %ebx
	pushl %ebx
	call print
	addl $4,%esp
	cmp %esi, %edi
	setle %bl
	movzbl %bl, %ebx
	pushl %ebx
	call print
	addl $4,%esp
	cmp %edi, %esi
	setl %bl
	movzbl %bl, %ebx
	cmpl $0, %ebx
	jle L1_terminate
	jmp L1_aint_gonna_happen
L1_aint_gonna_happen:
L1_terminate:
	pushl $85
	call print
	addl $4,%esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
