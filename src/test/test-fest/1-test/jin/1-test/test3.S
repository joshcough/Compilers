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
	movl $1, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	subl $2, %eax
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_fib:
	cmpl $2, %eax
	jl L1_base
	jmp L1_recur
L1_base:
	movl $31, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	movl $51, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
