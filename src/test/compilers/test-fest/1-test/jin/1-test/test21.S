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
	movl $18, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_add
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
L1_add:
	cmpl $20, %eax
	jl L1_add1
	jmp L1_add2
L1_add1:
	addl $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_add2:
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_sub
L1_Generated_Label_1:
	addl $2, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_sub:
	subl $10, %eax
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
