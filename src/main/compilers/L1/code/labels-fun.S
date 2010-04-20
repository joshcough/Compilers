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
	movl $2, %esi
L1_0:
	jmp L1_wefowifwef_wefowfoiwejf_AOIOAOIDJOAI_93493sdfij23nfsdUH
L1_wefowifwef_wefowfoiwejf_AOIOAOIDJOAI_93493sdfij23nfsdUH:
	jmp L1_112345
L1_112345:
	jmp L1_9
L1_9:
	jmp L1__
L1__:
	jmp L1___________________________________
L1___________________________________:
	jmp L1________________999
L1________________999:
	jmp L1________________aaa
L1________________aaa:
	jmp L1________________AAA
L1________________AAA:
	subl $1, %esi
	cmpl $0, %esi
	je L1__A_A_A_A_A_A_A_A_
	jmp L1_0
L1__A_A_A_A_A_A_A_A_:
	movl $3, %eax
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
