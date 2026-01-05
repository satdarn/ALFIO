.intel_syntax noprefix
.section .rodata
fmt_int:
      .string "%ld\n"
.text
.globl main
.extern printf
main:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     xor rax, rax
     mov QWORD PTR [rbp - 8], rax
     mov rcx, 0
     mov r9, rcx
     mov rcx, 1
     neg rcx
     mov r8, rcx
     mov rcx, 34
     mov rcx, rcx
     mov rcx, 7
     mov rdx, rcx
     mov rcx, 4096
     mov rsi, rcx
     mov rcx, 0
     mov rdi, rcx
     call mmap
     mov QWORD PTR [rbp - 8], rax
.L_WHILE1:
     mov rcx, 2048
     mov rdx, rcx
     mov rcx, QWORD PTR [rbp - 8]
     mov rsi, rcx
     mov rcx, 0
     mov rdi, rcx
     call read_string
     cmp rax, 0
     je .L_WHILE2
     mov rcx, 2048
     mov rdx, rcx
     mov rcx, QWORD PTR [rbp - 8]
     mov rsi, rcx
     mov rcx, 1
     mov rdi, rcx
     call write_string
     jmp .L_WHILE1
.L_WHILE2:
     mov rcx, 4096
     mov rsi, rcx
     mov rcx, QWORD PTR [rbp - 8]
     mov rdi, rcx
     call munmap
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
write_string:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     mov QWORD PTR [rbp - 32], rdx
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov QWORD PTR [rbp - 8], rcx
.L_WHILE3:
     mov ecx, DWORD PTR [rbp - 8]
     mov edx, DWORD PTR [rbp - 32]
     cmp rcx, rdx
     setbe al
     xor rcx, rcx
     mov rcx, al
     cmp rcx, 0
     je .L_WHILE4
     mov rdx, QWORD PTR [rbp - 24]
     mov r8d, DWORD PTR [rbp - 8]
     add rdx, r8
     mov cl, BYTE PTR [rdx]
     mov rdx, 0
     cmp rcx, rdx
     sete al
     xor rcx, rcx
     mov rcx, al
     cmp rcx, 0
     je .L_IF5
     jmp .L_WHILE4
     jmp .L_IF6
.L_IF5:
.L_IF6:
     mov ecx, DWORD PTR [rbp - 8]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_WHILE3
.L_WHILE4:
     mov ecx, DWORD PTR [rbp - 8]
     mov rdx, rcx
     mov rcx, QWORD PTR [rbp - 24]
     mov rsi, rcx
     mov ecx, DWORD PTR [rbp - 16]
     mov rdi, rcx
     mov rcx, 1
     mov rax, rcx
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
read_string:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov QWORD PTR [rbp - 24], rdx
     mov ecx, DWORD PTR [rbp - 24]
     mov rdx, rcx
     mov rcx, QWORD PTR [rbp - 16]
     mov rsi, rcx
     mov ecx, DWORD PTR [rbp - 8]
     mov rdi, rcx
     mov rcx, 0
     mov rax, rcx
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
mmap:
     push rbp
     mov rbp, rsp
     sub rsp, 48
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov QWORD PTR [rbp - 24], rdx
     mov QWORD PTR [rbp - 32], rcx
     mov QWORD PTR [rbp - 40], r8
     mov rcx, QWORD PTR [rbp - 48]
     mov r9, rcx
     mov ecx, DWORD PTR [rbp - 40]
     mov r8, rcx
     mov ecx, DWORD PTR [rbp - 32]
     mov r10, rcx
     mov ecx, DWORD PTR [rbp - 24]
     mov rdx, rcx
     mov rcx, QWORD PTR [rbp - 16]
     mov rsi, rcx
     mov rcx, QWORD PTR [rbp - 8]
     mov rdi, rcx
     mov rcx, 9
     mov rax, rcx
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
munmap:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov rcx, QWORD PTR [rbp - 16]
     mov rsi, rcx
     mov rcx, QWORD PTR [rbp - 8]
     mov rdi, rcx
     mov rcx, 11
     mov rax, rcx
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
