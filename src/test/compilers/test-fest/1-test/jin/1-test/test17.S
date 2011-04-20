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
	movl $15, %ecx
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_0:
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_fib:
L1_loop:
	cmpl %esi, %ebx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	cmpl $6, %ecx
	jl L1_base
	jmp L1_recure
L1_base:
	movl -4(%ebx), %edx
	imull $2, %edx
	addl $1, %edx
	movl %edx, 0(%ebx)
	addl $4, %ebx
	movl %ebp, %esp
	popl %ebp
	ret
L1_recure:
	subl $1, %ecx
	pushl $L1_Generated_Label_1
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib
L1_Generated_Label_1:
	pushl $L1_Generated_Label_2
	pushl %ebp
	movl %esp, %ebp
	jmp L1_fib1
L1_Generated_Label_2:
	jmp L1_loop
	movl %ebp, %esp
	popl %ebp
	ret
L1_done:
	movl %ebp, %esp
	popl %ebp
	ret
L1_fib1:
	movl -4(%ebx), %edx
	addl $2, %edx
	movl %edx, -4(%ebx)
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
