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
	movl $16, %eax
	pushl $L1_Generated_Label_0
	pushl %ebp
	movl %esp, %ebp
	jmp L1_mul
L1_Generated_Label_0:
	subl $1, %eax
	pushl %eax
	call print
	addl $4, %esp
	popl %ebp
	popl %edi
	popl %esi
	popl %ebx
	leave
	ret
L1_mul:
	cmpl $10, %eax
	jl L1_mul1
	jmp L1_mul2
L1_mul1:
	imull $2, %eax
	movl %ebp, %esp
	popl %ebp
	ret
L1_mul2:
	movl $1, %ecx
L1_loop:
	cmpl $5, %ecx
	jl L1_times
	jmp L1_result
L1_times:
	imull %ecx, %eax
	addl $1, %ecx
	jmp L1_loop
L1_result:
	movl %ebp, %esp
	popl %ebp
	ret
	.size	go, .-go
	.ident	"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2"
	.section	.note.GNU-stack,"",@progbits
