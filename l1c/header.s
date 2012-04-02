	.file	"prog.c"
	.text
.globl go
	.type	go, @function
go:
        # save caller's base pointer
        pushl   %ebp
        movl    %esp, %ebp

        # save callee-saved registers
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp

        # body begins with base and
        # stack pointers equal
        movl    %esp, %ebp

