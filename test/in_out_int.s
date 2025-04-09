.globl Bool..vtable
Bool..vtable:
	.quad string0
	.quad Bool..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
.globl IO..vtable
IO..vtable:
	.quad string1
	.quad IO..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad IO.in_int
	.quad IO.in_string
	.quad IO.out_int
	.quad IO.out_string
.globl Int..vtable
Int..vtable:
	.quad string2
	.quad Int..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
.globl Main..vtable
Main..vtable:
	.quad string3
	.quad Main..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad IO.in_int
	.quad IO.in_string
	.quad IO.out_int
	.quad IO.out_string
	.quad Main.main
.globl Object..vtable
Object..vtable:
	.quad string4
	.quad Object..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
.globl String..vtable
String..vtable:
	.quad string5
	.quad String..new
	.quad Object.abort
	.quad Object.copy
	.quad Object.type_name
	.quad String.concat
	.quad String.length
	.quad String.substr
.globl Bool..new
Bool..new:			## constructor for Bool
	pushq %rbp
	movq %rsp, %rbp
	movq $4, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $0, 0(%r12)
	movq $4, 8(%r12)
	movq $Bool..vtable, 16(%r12)
	movq $0, 24(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl IO..new
IO..new:			## constructor for IO
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $1, 0(%r12)
	movq $3, 8(%r12)
	movq $IO..vtable, 16(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl Int..new
Int..new:			## constructor for Int
	pushq %rbp
	movq %rsp, %rbp
	movq $4, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $2, 0(%r12)
	movq $4, 8(%r12)
	movq $Int..vtable, 16(%r12)
	movq $0, 24(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl Main..new
Main..new:			## constructor for Main
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $3, 0(%r12)
	movq $3, 8(%r12)
	movq $Main..vtable, 16(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl Object..new
Object..new:			## constructor for Object
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $4, 0(%r12)
	movq $3, 8(%r12)
	movq $Object..vtable, 16(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl String..new
String..new:			## constructor for String
	pushq %rbp
	movq %rsp, %rbp
	movq $4, %rdi
	movq $8, %rsi
	call calloc
	movq %rax, %r12
	movq $5, 0(%r12)
	movq $4, 8(%r12)
	movq $String..vtable, 16(%r12)
	movq $the.empty.string, 24(%r12)
	movq %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret

## USER METHOD BODIES BEGINS

## method definition of Main.main
.globl Main.main
Main.main:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	## stack room for temporaries: 1
	subq $8, %rsp
## start
	pushq %r12
	pushq %rbp
	pushq %r12
	movq 16(%r12), %r14
	movq 40(%r14), %r14
	call *%r14
	addq $8, %rsp
	popq %rbp
	popq %r12
	pushq %r12
	pushq %rbp
	pushq 8(%rbp)
	pushq %r12
	movq 16(%r12), %r14
	movq 56(%r14), %r14
	call *%r14
	addq $16, %rsp
	popq %rbp
	popq %r12
	movq %rbp, %rsp
	popq %rbp
	ret

## USER METHOD BODIES ENDS

## INTERNAL METHOD BODIES BEGINS

## method definition of IO.in_int
.globl IO.in_int
IO.in_int:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	## stack room for temporaries: 2
	movq $16, %r14
	subq %r14, %rsp
	## return address handling
	## method body begins
	## new Int
	pushq %rbp
	pushq %r12
	movq $Int..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq %r13, %r14
	## calloc input buffer
	movl $1, %esi
	movl $4096, %edi
	call calloc

	## read input via fgets
	movq %rax, %rdi
	movq $4096, %rsi
	movq stdin(%rip), %rdx

	## guarantee 16-byte alignment before call
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	call fgets
	movq %rax, %r15

	## r15 contains the string now
	movq %r15, %rdi
	movq $percent.d, %rsi
	movq %r13, %rdx

	## guarantee 16-byte alignment before call
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	call sscanf

	movq (%r13), %rax
	## rax contains the int now

	## store int into Int()
	movq %rax, 24(%r14)
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of IO.in_string
.globl IO.in_string
IO.in_string:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq %rbp
	pushq %r12
	movq $String..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq %r13, %r14
	## guarantee 16-byte alignment before call
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	call coolgetstr
	movq %rax, %r13
	movq %r13, 24(%r14)
	movq %r14, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of IO.out_int
.globl IO.out_int
IO.out_int:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	movq 24(%rbp), %r14
	movq 24(%r14), %r13
	movq $percent.ld, %rdi
	movl %r13d, %eax
	cdqe
	movq %rax, %rsi
	movl $0, %eax
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	call printf
	mov %r12, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of IO.out_string
.globl IO.out_string
IO.out_string:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	movq $16, %r14
	subq %r14, %rsp
	movq 24(%rbp), %r14
	movq 24(%r14), %r13
	## guarantee 16-byte alignment before call
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	movq %r13, %rdi
	call cooloutstr
	movq %r12, %r13
	ret
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of Object.abort
.globl Object.abort
Object.abort:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	movq $abort.string, %rdi
	call cooloutstr
	movl $0, %edi
	call exit
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of Object.copy
.globl Object.copy
Object.copy:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $16, %rsp
	movq 8(%r12), %r14
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	movq $8, %rsi
	movq %r14, %rdi
	movq %rax, %r13
	pushq %r13
.globl l1
l1:
	cmpq $0, %r14
	je l2
	movq 0(%r12), %r15
	movq %r15, 0(%r13)
	movq $8, %r15
	addq %r15, %r12
	addq %r15, %r13
	movq $1, %r15
	subq %r15, %r14
	jmp l1
.globl l2
l2:
	popq %r13
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of Object.type_name
.globl Object.type_name
Object.type_name:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	pushq %rbp
	pushq %r12
	movq $String..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq 16(%r12), %r14
	movq 0(%r14), %r14
	movq %r14, 24(%r13)
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of String.concat
.globl String.concat
String.concat:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	pushq %rbp
	pushq %r12
	movq $String..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq %r13, %r15
	movq 24(%rbp), %r14
	movq 24(%r14), %r14
	movq 24(%r12), %r13
	movq %r13, %rdi
	movq %r14, %rsi
	call coolstrcat
	movq %rax, %r13
	movq %r13, 24(%r15)
	movq %r15, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of String.length
.globl String.length
String.length:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	pushq %rbp
	pushq %r12
	movq $Int..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq %r13, %r14
	movq 24(%r12), %r13
	movq %r13, %rdi
	movl $0, %eax
	call coolstrlen
	movq %rax, %r13
	movq %r13, 24(%r14)
	movq %r14, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
## method definition of String.substr
.globl String.substr
String.substr:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %r12
	subq $8, %rsp
	pushq %rbp
	pushq %r12
	movq $String..new, %r14
	call *%r14
	popq %r12
	popq %rbp
	movq %r13, %r15
	movq 24(%rbp), %r14
	movq 24(%r14), %r14
	movq 32(%rbp), %r13
	movq 24(%r13), %r13
	movq 24(%r12), %r12
	movq %r12, %rdi
	movq %r13, %rsi
	movq %r14, %rdx
	call coolsubstr
	movq %rax, %r13
	cmpq $0, %r13
	jne l3
	movq $substr.error.string, %r13
	movq %r13, %rdi
	call cooloutstr
	movl $0, %edi
	call exit
.globl l3
l3:
	movq %r13, 24(%r15)
	movq %r15, %r13
	movq %rbp, %rsp
	popq %rbp
	ret
.globl cooloutstr
.type cooloutstr, @function
cooloutstr:
.LFB6:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L2
.L5:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L3
	movl	-4(%rbp), %eax
	cltq
	leaq	1(%rax), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$110, %al
	jne	.L3
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$10, %edi
	call	fputc@PLT
	addl	$2, -4(%rbp)
	jmp	.L2
.L3:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$92, %al
	jne	.L4
	movl	-4(%rbp), %eax
	cltq
	leaq	1(%rax), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$116, %al
	jne	.L4
	movq	stdout(%rip), %rax
	movq	%rax, %rsi
	movl	$9, %edi
	call	fputc@PLT
	addl	$2, -4(%rbp)
	jmp	.L2
.L4:
	movq	stdout(%rip), %rdx
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	-24(%rbp), %rax
	addq	%rcx, %rax
	movzbl	(%rax), %eax
	movsbl	%al, %eax
	movq	%rdx, %rsi
	movl	%eax, %edi
	call	fputc@PLT
	addl	$1, -4(%rbp)
.L2:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	testb	%al, %al
	jne	.L5
	movq	stdout(%rip), %rax
	movq	%rax, %rdi
	call	fflush@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	cooloutstr, .-cooloutstr
.globl coolstrlen
.type coolstrlen, @function
coolstrlen:
.LFB7:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq %rsp, %rbp
	.cfi_def_cfa_register 6
	movq %rdi, -24(%rbp)
	movl $0, -4(%rbp)
	jmp .L7
.L8:
	movl -4(%rbp), %eax
	addl $1, %eax
	movl %eax, -4(%rbp)
.L7:
	movl -4(%rbp), %eax
	movl %eax, %edx
	movq -24(%rbp), %rax
	addq %rdx, %rax
	movzbl (%rax), %eax
	testb %al, %al
	jne .L8
	movl -4(%rbp), %eax
	popq %rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	coolstrlen, .-coolstrlen
	.section	.rodata
.LC0:
.string	"%s%s"
.text
.globl	coolstrcat
.type	coolstrcat, @function
coolstrcat:
.LFB8:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	cmpq	$0, -40(%rbp)
	jne	.L11
	movq	-48(%rbp), %rax
	jmp	.L12
.L11:
	cmpq	$0, -48(%rbp)
	jne	.L13
	movq	-40(%rbp), %rax
	jmp	.L12
.L13:
	movq	-40(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, %ebx
	movq	-48(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	addl	%ebx, %eax
	addl	$1, %eax
	movl	%eax, -28(%rbp)
	movl	-28(%rbp), %eax
	cltq
	movl	$1, %esi
	movq	%rax, %rdi
	call	calloc@PLT
	movq	%rax, -24(%rbp)
	movl	-28(%rbp), %eax
	movslq	%eax, %rsi
	movq	-48(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rcx, %r8
	movq	%rdx, %rcx
	leaq	.LC0(%rip), %rdx
	movq	%rax, %rdi
	movl	$0, %eax
	call	snprintf@PLT
	movq	-24(%rbp), %rax
.L12:
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	coolstrcat, .-coolstrcat
.globl	coolgetstr
.type	coolgetstr, @function
coolgetstr:
.LFB9:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	xorl	%eax, %eax
	movq	$0, -32(%rbp)
	movq	$0, -24(%rbp)
	movq	stdin(%rip), %rdx
	leaq	-24(%rbp), %rcx
	leaq	-32(%rbp), %rax
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	getline@PLT
	movq	%rax, -16(%rbp)
	cmpq	$-1, -16(%rbp)
	je	.L15
	movq	-32(%rbp), %rax
	testq	%rax, %rax
	jne	.L16
.L15:
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	free@PLT
	movl	$1, %edi
	call	malloc@PLT
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movb	$0, (%rax)
	jmp	.L17
.L16:
	movq	-16(%rbp), %rdx
	movq	-32(%rbp), %rax
	movl	$0, %esi
	movq	%rax, %rdi
	call	memchr@PLT
	testq	%rax, %rax
	je	.L18
	movq	-32(%rbp), %rax
	movb	$0, (%rax)
	jmp	.L17
.L18:
	movq	-32(%rbp), %rdx
	movq	-16(%rbp), %rax
	subq	$1, %rax
	addq	%rdx, %rax
	movzbl	(%rax), %eax
	cmpb	$10, %al
	jne	.L17
	movq	-32(%rbp), %rdx
	subq	$1, -16(%rbp)
	movq	-16(%rbp), %rax
	addq	%rdx, %rax
	movb	$0, (%rax)
.L17:
	movq	-32(%rbp), %rax
	movq	-8(%rbp), %rdx
	subq	%fs:40, %rdx
	je	.L20
	call	__stack_chk_fail@PLT
.L20:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	coolgetstr, .-coolgetstr
.globl	coolsubstr
.type	coolsubstr, @function
coolsubstr:
.LFB10:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	coolstrlen
	movl	%eax, -4(%rbp)
	cmpq	$0, -32(%rbp)
	js	.L22
	cmpq	$0, -40(%rbp)
	js	.L22
	movq	-32(%rbp), %rdx
	movq	-40(%rbp), %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	cmpq	%rax, %rdx
	jle	.L23
.L22:
	movl	$0, %eax
	jmp	.L24
.L23:
	movq	-40(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rdx
	addq	%rcx, %rdx
	movq	%rax, %rsi
	movq	%rdx, %rdi
	call	strndup@PLT
.L24:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	coolsubstr, .-coolsubstr

## INTERNAL METHOD BODIES END
.globl the.empty.string
the.empty.string:	## ""
.byte 0

.globl abort.string
abort.string:	## "abort\n"
.byte  97 # a
.byte  98 # b
.byte 111 # o
.byte 114 # r
.byte 116 # t
.byte  92 # \
.byte 110 # n
.byte 0

.globl string3
string3:	## "Main"
.byte  77 # M
.byte  97 # a
.byte 105 # i
.byte 110 # n
.byte 0

.globl percent.d
percent.d:	## "%d"
.byte  37 # %
.byte 100 # d
.byte 0

.globl string1
string1:	## "IO"
.byte  73 # I
.byte  79 # O
.byte 0

.globl string5
string5:	## "String"
.byte  83 # S
.byte 116 # t
.byte 114 # r
.byte 105 # i
.byte 110 # n
.byte 103 # g
.byte 0

.globl string2
string2:	## "Int"
.byte  73 # I
.byte 110 # n
.byte 116 # t
.byte 0

.globl string4
string4:	## "Object"
.byte  79 # O
.byte  98 # b
.byte 106 # j
.byte 101 # e
.byte  99 # c
.byte 116 # t
.byte 0

.globl percent.ld
percent.ld:	## "%ld"
.byte  37 # %
.byte 108 # l
.byte 100 # d
.byte 0

.globl string0
string0:	## "Bool"
.byte  66 # B
.byte 111 # o
.byte 111 # o
.byte 108 # l
.byte 0

.globl substr.error.string
substr.error.string:	## "ERROR: 0: Exception: String.substr out of range\n"
.byte  69 # E
.byte  82 # R
.byte  82 # R
.byte  79 # O
.byte  82 # R
.byte  58 # :
.byte  32 #  
.byte  48 # 0
.byte  58 # :
.byte  32 #  
.byte  69 # E
.byte 120 # x
.byte  99 # c
.byte 101 # e
.byte 112 # p
.byte 116 # t
.byte 105 # i
.byte 111 # o
.byte 110 # n
.byte  58 # :
.byte  32 #  
.byte  83 # S
.byte 116 # t
.byte 114 # r
.byte 105 # i
.byte 110 # n
.byte 103 # g
.byte  46 # .
.byte 115 # s
.byte 117 # u
.byte  98 # b
.byte 115 # s
.byte 116 # t
.byte 114 # r
.byte  32 #  
.byte 111 # o
.byte 117 # u
.byte 116 # t
.byte  32 #  
.byte 111 # o
.byte 102 # f
.byte  32 #  
.byte 114 # r
.byte  97 # a
.byte 110 # n
.byte 103 # g
.byte 101 # e
.byte  92 # \
.byte 110 # n
.byte 0


## LT_HANDLER
.globl lt_handler
lt_handler:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %r14
	movq 16(%rbp), %r15
	cmpq %r14, %r15
	jl lt_true
	movq $0, %rax
	jmp lt_handler_end
.globl lt_true
lt_true:
	movq $1, %rax
	jmp lt_handler_end
.globl lt_handler_end
lt_handler_end:
	movq %rbp, %rsp
	popq %rbp
	ret

## LE_HANDLER
.globl le_handler
le_handler:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %r14
	movq 16(%rbp), %r15
	cmpq %r14, %r15
	jle le_true
	movq $0, %rax
	jmp le_handler_end
.globl le_true
le_true:
	movq $1, %rax
	jmp le_handler_end
.globl le_handler_end
le_handler_end:
	movq %rbp, %rsp
	popq %rbp
	ret

## EQ_HANDLER
.globl eq_handler
eq_handler:
	pushq %rbp
	movq %rsp, %rbp
	movq 24(%rbp), %r14
	movq 16(%rbp), %r15
	cmpq %r14, %r15
	je eq_true
	movq $0, %rax
	jmp eq_handler_end
.globl eq_true
eq_true:
	movq $1, %rax
	jmp eq_handler_end
.globl eq_handler_end
eq_handler_end:
	movq %rbp, %rsp
	popq %rbp
	ret

## PROGRAM BEGINS HERE
.globl start
start:
.globl main
.type main, @function
main:
	movq $Main..new, %r14
	pushq %rbp
	call *%r14
	pushq %rbp
	pushq %r13
	movq $Main.main, %r14
	call *%r14
	## guarantee 16-byte alignment before call
	andq $0xFFFFFFFFFFFFFFF0, %rsp
	movl $0, %edi
	call exit
