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
	movl $10, %eax
	movl %eax, %ebx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fac
L1_Generated_Label_0:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_fac:
	cmpl $1, %ebx
	je L1_base
	jmp L1_recure
L1_base:
	imull $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_recure:
	movl %eax, %edx
	subl $1, %eax
	imull %edx, %eax
	subl $1, %ebx
	subl $1, %eax
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
