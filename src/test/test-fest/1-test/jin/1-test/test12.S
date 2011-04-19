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
	subl $8, %esp
	cmpl %esi, %ebx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	movl -4(%ebx), %edx
	subl $1, %edx
	movl %edx, -4(%ebp)
	addl $1, %edx
	imull %edx, %edx
	movl %eax, -8(%ebp)
	movl -4(%ebp), %eax
	imull %eax, %eax
	subl %eax, %edx
	movl -8(%ebp), %eax
	movl %edx, 0(%ebx)
	addl $4, %ebx
	addl $8, %esp
	jmp L1_loop
L1_done:
	pushl %edi
	call print
	addl $4, %esp
	addl $8, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
