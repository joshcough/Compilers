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
	movl $-1, %esi
	pushl %esi
	call print
	addl $4,%esp
	movl $-3, %edi
	pushl %edi
	call print
	addl $4,%esp
	cmpl %esi, %edi
	jl L1_end
	jmp L1_bad
L1_bad:
	pushl %esi
	call print
	addl $4,%esp
L1_end:
	pushl %edi
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
