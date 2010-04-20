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
	movl $7, %eax
	sarl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $15, %eax
	sarl $2, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $3, %eax
	sall $1, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $3, %eax
	sall $2, %eax
	addl $3, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $3, %eax
	andl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $27, %eax
	andl $13, %eax
	pushl %eax
	call print
	addl $4,%esp
	movl $27, %eax
	movl $13, %ebx
	andl %ebx, %eax
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
