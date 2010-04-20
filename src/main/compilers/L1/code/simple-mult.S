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
	movl $5, %eax
	imull $2, %eax
	subl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $5, %eax
	imull $3, %eax
	subl $2, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $5, %eax
	imull $4, %eax
	subl $3, %eax
	pushl %eax
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
