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
	movl $8, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	addl %eax, %eax
	addl $1, %eax
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
	subl $8, %esp
	cmpl $2, %eax
	jl L1_base
	jmp L1_recur
L1_base:
	movl $1, %eax
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
L1_recur:
	movl %eax, -4(%ebp)
	subl $1, %eax
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_1:
	movl %eax, -8(%ebp)
	movl -4(%ebp), %eax
	subl $2, %eax
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_2:
	movl -8(%ebp), %esi
	addl %esi, %eax
	addl $8, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
