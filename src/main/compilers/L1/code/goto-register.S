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
	movl $5, %ebx
	movl $1, %ecx
L1_boogers:
	movl $L1_boogers, %eax
	subl $2, %ebx
	addl $2, %ecx
	cmpl $2, %ebx
	jl L1_the_end
	jmp L1_the_next
L1_the_next:
	jmp *%eax
L1_the_end:
	pushl %ecx
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
