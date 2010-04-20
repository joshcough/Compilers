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
	movl $7, %esi
	movl $8, %edi
	cmp %edi, %esi
	setl %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4,%esp
	cmp %esi, %edi
	setl %al
	movzbl %al, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	cmp %edi, %esi
	setle %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4,%esp
	cmp %esi, %edi
	setle %al
	movzbl %al, %eax
	addl $1, %eax
	pushl %eax
	call print
	addl $4,%esp
	cmp %esi, %esi
	sete %al
	movzbl %al, %eax
	pushl %eax
	call print
	addl $4,%esp
	cmp %esi, %edi
	sete %al
	movzbl %al, %eax
	addl $1, %eax
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
