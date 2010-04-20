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
	pushl $101
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl $50, %ebx
	movl $1, %edx
L1_filler:
	movl %ebx, %ecx
	imull $4, %ecx
	addl %edi, %ecx
	movl %edx, %esi
	imull $2, %esi
	addl $1, %esi
	movl %esi, 0(%ecx)
	subl $1, %ebx
	addl $1, %edx
	cmpl $1, %ebx
	jl L1_printer
	jmp L1_filler
L1_printer:
	movl $1, %ebx
L1_printer2:
	movl %ebx, %esi
	imull $4, %esi
	addl %edi, %esi
	movl 0(%esi), %esi
	pushl %esi
	call print
	addl $4,%esp
	movl $51, %edx
	addl $1, %ebx
	cmpl %edx, %ebx
	jl L1_printer2
	jmp L1_ender
L1_ender:
	pushl %edi
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
