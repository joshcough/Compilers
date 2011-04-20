	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
	pushl %ebp
	movl %esp, %ebp
	pushl %ebx
	pushl %esi
	pushl %edi
	pushl %ebp
	movl	%esp, %ebp
	movl $L1_x, %edi
	pushl $3
	call print
	addl $4, %esp
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp *%edi
L1_Generated_Label_0:
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_x:
	pushl $3
	call print
	addl $4, %esp
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
