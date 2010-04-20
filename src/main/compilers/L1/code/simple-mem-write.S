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
	pushl $3
	pushl $9
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl $5, 4(%edi)
	movl $7, 8(%edi)
	movl $9, 12(%edi)
	movl $11, 16(%edi)
	movl 4(%edi), %esi
	pushl %esi
	call print
	addl $4,%esp
	movl 8(%edi), %esi
	pushl %esi
	call print
	addl $4,%esp
	movl 12(%edi), %esi
	pushl %esi
	call print
	addl $4,%esp
	movl 16(%edi), %esi
	pushl %esi
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
