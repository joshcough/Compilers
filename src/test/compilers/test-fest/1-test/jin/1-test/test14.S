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
	movl $1, %ebx
	movl $3, %eax
L1_loop:
	cmpl $5, %ebx
	jl L1_keep_going
	jmp L1_done
L1_keep_going:
	cmpl $1, %eax
	je L1_step_1
	jmp L1_step_2
L1_step_1:
	addl $10, %eax
	addl $1, %ebx
	jmp L1_loop
L1_step_2:
	addl $4, %eax
	addl $1, %ebx
	jmp L1_loop
L1_done:
	pushl %eax
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
