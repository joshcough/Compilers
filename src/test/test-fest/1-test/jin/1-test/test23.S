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
	movl $21, %esi
	pushl $3
	pushl %esi
	call allocate
	addl $8, %esp
	movl %eax, %edi
	movl %edi, %ebx
	addl $12, %ebx
	imull $2, %esi
	addl $2, %esi
	addl %edi, %esi
L1_loop:
	cmpl %esi, %ebx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl -4(%ebx), %edx
	movl 0(%ebx), %ecx
	imull %ecx, %edx
	movl %edx, 0(%ebx)
	addl $4, %ebx
	jmp L1_loop
L1_done:
	pushl %edi
	call print
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
