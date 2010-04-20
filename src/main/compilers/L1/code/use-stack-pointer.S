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
	subl $12, %esp
	movl $3, -4(%ebp)
	movl $85, -8(%ebp)
	movl $109, -12(%ebp)
	movl 0(%esp), %ebx
	pushl %ebx
	call print
	addl $4,%esp
	addl $4, %esp
	movl 0(%esp), %ebx
	pushl %ebx
	call print
	addl $4,%esp
	addl $4, %esp
	movl 0(%esp), %ebx
	pushl %ebx
	call print
	addl $4,%esp
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
