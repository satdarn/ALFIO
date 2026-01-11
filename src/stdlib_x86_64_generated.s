.global print_int
.global print
.global print_hex
.global open
.global integer_to_ascii
.global reverse
.global is_digit
.global read
.global read_line
.global memcpy
.global write
.global memeql
.global strcmp
.global exp
.global exit
.global byte_out
.global mmap
.global ascii_to_integer
.global byte_in
.global munmap
.global memset
.global strlen
.global close
.global strcpy
.global print_string
.intel_syntax noprefix
.text
exit:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     movsxd r11, DWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 60
     mov rax, r11
     syscall
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
open:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov QWORD PTR [rbp - 24], rdx
     mov r11, QWORD PTR [rbp - 24]
     mov rdx, r11
     movsxd r11, DWORD PTR [rbp - 16]
     mov rsi, r11
     mov r11, QWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 2
     mov rax, r11
     syscall
     movsxd rax, eax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
close:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     movsxd r11, DWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 3
     mov rax, r11
     syscall
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
write:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov QWORD PTR [rbp - 24], rdx
     movsxd r11, DWORD PTR [rbp - 24]
     mov rdx, r11
     mov r11, QWORD PTR [rbp - 16]
     mov rsi, r11
     movsxd r11, DWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 1
     mov rax, r11
     syscall
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
byte_out:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     mov r10, 1
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 8]
     mov rsi, r10
     mov r10, 1
     mov rdi, r10
     call write
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
byte_in:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     xor rax, rax
     mov BYTE PTR [rbp - 8], al
     mov rcx, 0
     mov BYTE PTR [rbp - 8], cl
     mov r10, 1
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 8]
     mov rsi, r10
     mov r10, 0
     mov rdi, r10
     call read
     mov rcx, 0
     cmp rax, rcx
     setne al
     movzx rax, al
     cmp al, 0
     je .L_IF1
     movzx rcx, BYTE PTR [rbp - 8]
     movzx rax, cl
     mov rsp, rbp
     pop rbp
     ret
     jmp .L_IF2
.L_IF1:
.L_IF2:
     mov rcx, 0
     movzx rax, cl
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
print:
     push rbp
     mov rbp, rsp
     sub rsp, 272
     mov QWORD PTR [rbp - 264], rdi
     xor rax, rax
     mov r10, 10
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 256]
     mov rsi, r10
     movsxd r10, DWORD PTR [rbp - 264]
     mov rdi, r10
     call integer_to_ascii
     mov rdi, rax
     call print_string
     mov r10, 10
     mov rdi, r10
     call byte_out
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
read:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 8], rdi
     mov QWORD PTR [rbp - 16], rsi
     mov QWORD PTR [rbp - 24], rdx
     movsxd r11, DWORD PTR [rbp - 24]
     mov rdx, r11
     mov r11, QWORD PTR [rbp - 16]
     mov rsi, r11
     movsxd r11, DWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 0
     mov rax, r11
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
read_line:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 24], rdi
     mov QWORD PTR [rbp - 32], rsi
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov BYTE PTR [rbp - 16], al
     mov rcx, QWORD PTR [rbp - 24]
     mov rdx, 0
     cmp rcx, rdx
     sete al
     movzx rcx, al
     movsxd rdx, DWORD PTR [rbp - 32]
     mov rsi, 1
     cmp rdx, rsi
     setb al
     movzx rdx, al
     cmp rcx, 1
     je .L_OR5
     cmp rdx, 1
     je .L_OR5
     mov rcx, 0 
     jmp .L_OR6
.L_OR5:
     mov rcx, 1
.L_OR6:     cmp cl, 0
     je .L_IF3
     mov rcx, 1
     neg rcx
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     jmp .L_IF4
.L_IF3:
.L_IF4:
     mov rcx, 0
     mov BYTE PTR [rbp - 16], cl
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP7:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rdx, DWORD PTR [rbp - 32]
     mov rsi, 1
     sub rdx, rsi
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP8
     mov r10, 1
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 16]
     mov rsi, r10
     mov r10, 0
     mov rdi, r10
     call read
     mov rcx, 1
     cmp rax, rcx
     setne al
     movzx rax, al
     cmp al, 0
     je .L_IF9
     jmp .L_LOOP8
     jmp .L_IF10
.L_IF9:
.L_IF10:
     movzx r10, BYTE PTR [rbp - 16]
     mov rdi, r10
     call byte_out
     movzx rcx, BYTE PTR [rbp - 16]
     mov rdx, 10
     cmp rcx, rdx
     sete al
     movzx rcx, al
     cmp cl, 0
     je .L_IF11
     jmp .L_LOOP8
     jmp .L_IF12
.L_IF11:
.L_IF12:
     movzx rcx, BYTE PTR [rbp - 16]
     mov rdx, QWORD PTR [rbp - 24]
     movsxd rsi, DWORD PTR [rbp - 8]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_LOOP7
.L_LOOP8:
     mov rcx, 0
     mov rdx, QWORD PTR [rbp - 24]
     movsxd rsi, DWORD PTR [rbp - 8]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rax, ecx
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
     mov QWORD PTR [rbp - 48], r9
     mov r11, QWORD PTR [rbp - 48]
     mov r9, r11
     movsxd r11, DWORD PTR [rbp - 40]
     mov r8, r11
     movsxd r11, DWORD PTR [rbp - 32]
     mov r10, r11
     movsxd r11, DWORD PTR [rbp - 24]
     mov rdx, r11
     mov r11, QWORD PTR [rbp - 16]
     mov rsi, r11
     mov r11, QWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 9
     mov rax, r11
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
     mov r11, QWORD PTR [rbp - 16]
     mov rsi, r11
     mov r11, QWORD PTR [rbp - 8]
     mov rdi, r11
     mov r11, 11
     mov rax, r11
     syscall
     mov rax, rax
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
memcpy:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     mov QWORD PTR [rbp - 32], rdx
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP13:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rdx, DWORD PTR [rbp - 32]
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP14
     mov rdx, QWORD PTR [rbp - 24]
     movsxd rsi, DWORD PTR [rbp - 8]
     add rdx, rsi
     mov rsi, QWORD PTR [rbp - 24]
     movsxd rdi, DWORD PTR [rbp - 8]
     add rsi, rdi
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov rsi, QWORD PTR [rbp - 16]
     movsxd rdi, DWORD PTR [rbp - 8]
     add rsi, rdi
     mov BYTE PTR [rsi], cl
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rsi, 1
     add rcx, rsi
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_LOOP13
.L_LOOP14:
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
memset:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     mov QWORD PTR [rbp - 32], rdx
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP15:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rdx, DWORD PTR [rbp - 32]
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP16
     movzx rcx, BYTE PTR [rbp - 24]
     mov rdx, QWORD PTR [rbp - 16]
     movsxd rsi, DWORD PTR [rbp - 8]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_LOOP15
.L_LOOP16:
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
memeql:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     mov QWORD PTR [rbp - 32], rdx
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP17:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rdx, DWORD PTR [rbp - 32]
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP18
     mov rdx, QWORD PTR [rbp - 16]
     mov rsi, QWORD PTR [rbp - 16]
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov rdi, QWORD PTR [rbp - 24]
     mov r8, QWORD PTR [rbp - 24]
     xor rsi, rsi
     mov sil, BYTE PTR [r8]
     cmp rcx, rsi
     setne al
     movzx rcx, al
     cmp cl, 0
     je .L_IF19
     mov r11, QWORD PTR [rbp - 16]
     mov r12, QWORD PTR [rbp - 16]
     xor r10, r10
     mov r10b, BYTE PTR [r12]
     mov rdi, r10
     call byte_out
     mov r10, 10
     mov rdi, r10
     call byte_out
     mov r12, QWORD PTR [rbp - 24]
     mov r13, QWORD PTR [rbp - 24]
     xor r10, r10
     mov r10b, BYTE PTR [r13]
     mov rdi, r10
     call byte_out
     mov rcx, 0
     movzx rax, cl
     mov rsp, rbp
     pop rbp
     ret
     jmp .L_IF20
.L_IF19:
.L_IF20:
     mov rcx, QWORD PTR [rbp - 16]
     mov rdx, 1
     add rcx, rdx
     mov QWORD PTR [rbp - 16], rcx
     mov rcx, QWORD PTR [rbp - 24]
     mov rdx, 1
     add rcx, rdx
     mov QWORD PTR [rbp - 24], rcx
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_LOOP17
.L_LOOP18:
     mov rcx, 1
     movzx rax, cl
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
strlen:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 16], rdi
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP21:
     mov rdx, QWORD PTR [rbp - 16]
     mov rsi, QWORD PTR [rbp - 16]
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov rsi, 0
     cmp rcx, rsi
     setne al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP22
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rsi, 1
     add rcx, rsi
     mov DWORD PTR [rbp - 8], ecx
     mov rcx, QWORD PTR [rbp - 16]
     mov rsi, 1
     add rcx, rsi
     mov QWORD PTR [rbp - 16], rcx
     jmp .L_LOOP21
.L_LOOP22:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
strcmp:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP23:
     mov rdx, QWORD PTR [rbp - 16]
     mov rsi, QWORD PTR [rbp - 16]
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov rsi, 0
     cmp rcx, rsi
     setne al
     movzx rcx, al
     mov rdi, QWORD PTR [rbp - 16]
     mov r8, QWORD PTR [rbp - 16]
     xor rsi, rsi
     mov sil, BYTE PTR [r8]
     mov r8, 0
     cmp rsi, r8
     setne al
     movzx rsi, al
     cmp rcx, 0
     je .L_AND27
     cmp rsi, 0
     je .L_AND27
     mov rcx, 1 
     jmp .L_AND26
.L_AND27:
     mov rcx, 0
.L_AND26:
     cmp cl, 0
     je .L_LOOP24
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rsi, 0
     cmp rcx, rsi
     setne al
     movzx rcx, al
     cmp cl, 0
     je .L_IF27
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     jmp .L_IF28
.L_IF27:
.L_IF28:
     mov rsi, QWORD PTR [rbp - 16]
     mov r8, QWORD PTR [rbp - 16]
     xor rcx, rcx
     mov cl, BYTE PTR [r8]
     mov r9, QWORD PTR [rbp - 24]
     mov r10, QWORD PTR [rbp - 24]
     xor r8, r8
     mov r8b, BYTE PTR [r10]
     sub rcx, r8
     mov DWORD PTR [rbp - 8], ecx
     mov rcx, QWORD PTR [rbp - 16]
     mov r8, 1
     add rcx, r8
     mov QWORD PTR [rbp - 16], rcx
     mov rcx, QWORD PTR [rbp - 24]
     mov r8, 1
     add rcx, r8
     mov QWORD PTR [rbp - 24], rcx
     jmp .L_LOOP23
.L_LOOP24:
     movsxd rcx, DWORD PTR [rbp - 8]
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
strcpy:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 16], rdi
     mov QWORD PTR [rbp - 24], rsi
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov r10, QWORD PTR [rbp - 24]
     mov rdi, r10
     call strlen
     mov DWORD PTR [rbp - 8], eax
     movsxd r10, DWORD PTR [rbp - 8]
     mov rdx, r10
     mov r10, QWORD PTR [rbp - 24]
     mov rsi, r10
     mov r10, QWORD PTR [rbp - 16]
     mov rdi, r10
     call memcpy
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
print_string:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     mov r10, QWORD PTR [rbp - 8]
     mov rdi, r10
     call strlen
     mov rdx, rax
     mov rcx, QWORD PTR [rbp - 8]
     mov rsi, rcx
     mov rcx, 1
     mov rdi, rcx
     call write
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
print_int:
     push rbp
     mov rbp, rsp
     sub rsp, 272
     mov QWORD PTR [rbp - 264], rdi
     xor rax, rax
     mov r10, 10
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 256]
     mov rsi, r10
     movsxd r10, DWORD PTR [rbp - 264]
     mov rdi, r10
     call integer_to_ascii
     mov rdi, rax
     call print_string
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
print_hex:
     push rbp
     mov rbp, rsp
     sub rsp, 272
     mov QWORD PTR [rbp - 264], rdi
     xor rax, rax
     mov r10, 16
     mov rdx, r10
     lea r10, QWORD PTR [rbp - 256]
     mov rsi, r10
     movsxd r10, DWORD PTR [rbp - 264]
     mov rdi, r10
     call integer_to_ascii
     mov rdi, rax
     call print_string
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
is_digit:
     push rbp
     mov rbp, rsp
     sub rsp, 16
     mov QWORD PTR [rbp - 8], rdi
     mov rcx, 48
     movzx rdx, BYTE PTR [rbp - 8]
     cmp rcx, rdx
     setbe al
     movzx rcx, al
     movzx rdx, BYTE PTR [rbp - 8]
     mov rsi, 57
     cmp rdx, rsi
     setbe al
     movzx rdx, al
     cmp rcx, 0
     je .L_AND31
     cmp rdx, 0
     je .L_AND31
     mov rcx, 1 
     jmp .L_AND30
.L_AND31:
     mov rcx, 0
.L_AND30:
     movzx rax, cl
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
exp:
     push rbp
     mov rbp, rsp
     sub rsp, 32
     mov QWORD PTR [rbp - 24], rdi
     mov QWORD PTR [rbp - 32], rsi
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov DWORD PTR [rbp - 16], eax
     mov rcx, 1
     mov DWORD PTR [rbp - 16], ecx
     movsxd rcx, DWORD PTR [rbp - 32]
     mov DWORD PTR [rbp - 8], ecx
.L_LOOP31:
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 0
     cmp rcx, rdx
     setne al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP32
     movsxd rcx, DWORD PTR [rbp - 16]
     movsxd rdx, DWORD PTR [rbp - 24]
     push rax
     push rbx 
     mov rax, rcx
     mov rbx, rdx
     imul rax, rbx 
     mov rcx, rax
     pop rbx
     pop rax
     mov DWORD PTR [rbp - 16], ecx
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 1
     sub rcx, rdx
     mov DWORD PTR [rbp - 8], ecx
     jmp .L_LOOP31
.L_LOOP32:
     movsxd rcx, DWORD PTR [rbp - 16]
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
ascii_to_integer:
     push rbp
     mov rbp, rsp
     sub rsp, 64
     mov QWORD PTR [rbp - 56], rdi
     xor rax, rax
     mov QWORD PTR [rbp - 8], rax
     mov DWORD PTR [rbp - 16], eax
     mov BYTE PTR [rbp - 24], al
     mov DWORD PTR [rbp - 32], eax
     mov BYTE PTR [rbp - 40], al
     mov DWORD PTR [rbp - 48], eax
     mov rcx, QWORD PTR [rbp - 56]
     mov QWORD PTR [rbp - 8], rcx
     mov rcx, 0
     mov DWORD PTR [rbp - 32], ecx
     mov r10, QWORD PTR [rbp - 56]
     mov rdi, r10
     call strlen
     mov DWORD PTR [rbp - 16], eax
     mov rcx, 0
     mov BYTE PTR [rbp - 40], cl
     mov rdx, QWORD PTR [rbp - 8]
     mov rsi, QWORD PTR [rbp - 8]
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov rsi, 45
     cmp rcx, rsi
     sete al
     movzx rcx, al
     cmp cl, 0
     je .L_IF33
     mov rcx, 1
     mov BYTE PTR [rbp - 40], cl
     mov rcx, QWORD PTR [rbp - 8]
     mov rsi, 1
     add rcx, rsi
     mov QWORD PTR [rbp - 8], rcx
     movsxd rcx, DWORD PTR [rbp - 16]
     mov rsi, 1
     sub rcx, rsi
     mov DWORD PTR [rbp - 16], ecx
     jmp .L_IF34
.L_IF33:
     mov rsi, QWORD PTR [rbp - 8]
     mov rdi, QWORD PTR [rbp - 8]
     xor rcx, rcx
     mov cl, BYTE PTR [rdi]
     mov rdi, 43
     cmp rcx, rdi
     sete al
     movzx rcx, al
     cmp cl, 0
     je .L_IF35
     mov rcx, QWORD PTR [rbp - 8]
     mov rdi, 1
     add rcx, rdi
     mov QWORD PTR [rbp - 8], rcx
     movsxd rcx, DWORD PTR [rbp - 16]
     mov rdi, 1
     sub rcx, rdi
     mov DWORD PTR [rbp - 16], ecx
     jmp .L_IF36
.L_IF35:
.L_IF36:
.L_IF34:
     mov rcx, 0
     mov DWORD PTR [rbp - 48], ecx
.L_LOOP37:
     movsxd rcx, DWORD PTR [rbp - 48]
     movsxd rdi, DWORD PTR [rbp - 16]
     cmp rcx, rdi
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP38
     mov rdi, QWORD PTR [rbp - 8]
     movsxd r8, DWORD PTR [rbp - 48]
     add rdi, r8
     mov r8, QWORD PTR [rbp - 8]
     movsxd r9, DWORD PTR [rbp - 48]
     add r8, r9
     xor rcx, rcx
     mov cl, BYTE PTR [r8]
     mov BYTE PTR [rbp - 24], cl
     movzx r10, BYTE PTR [rbp - 24]
     mov rdi, r10
     call is_digit
     cmp al, 0
     je .L_IF39
     movsxd rcx, DWORD PTR [rbp - 32]
     mov rdx, 10
     push rax
     push rbx 
     mov rax, rcx
     mov rbx, rdx
     imul rax, rbx 
     mov rcx, rax
     pop rbx
     pop rax
     movzx rdx, BYTE PTR [rbp - 24]
     mov rsi, 48
     sub rdx, rsi
     add rcx, rdx
     mov DWORD PTR [rbp - 32], ecx
     jmp .L_IF40
.L_IF39:
     jmp .L_LOOP38
.L_IF40:
     movsxd rcx, DWORD PTR [rbp - 48]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 48], ecx
     jmp .L_LOOP37
.L_LOOP38:
     movzx rcx, BYTE PTR [rbp - 40]
     cmp rcx, 0
     je .L_IF41
     movsxd rcx, DWORD PTR [rbp - 32]
     neg rcx
     mov DWORD PTR [rbp - 32], ecx
     jmp .L_IF42
.L_IF41:
.L_IF42:
     movsxd rcx, DWORD PTR [rbp - 32]
     movsxd rax, ecx
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
integer_to_ascii:
     push rbp
     mov rbp, rsp
     sub rsp, 48
     mov QWORD PTR [rbp - 32], rdi
     mov QWORD PTR [rbp - 40], rsi
     mov QWORD PTR [rbp - 48], rdx
     xor rax, rax
     mov DWORD PTR [rbp - 8], eax
     mov DWORD PTR [rbp - 16], eax
     mov BYTE PTR [rbp - 24], al
     mov rcx, 0
     mov DWORD PTR [rbp - 16], ecx
     mov rcx, 0
     mov BYTE PTR [rbp - 24], cl
     movsxd rcx, DWORD PTR [rbp - 32]
     mov rdx, 0
     cmp rcx, rdx
     sete al
     movzx rcx, al
     cmp cl, 0
     je .L_IF43
     mov rcx, 48
     mov rdx, QWORD PTR [rbp - 40]
     mov BYTE PTR [rdx], cl
     mov rcx, 0
     mov rdx, QWORD PTR [rbp - 40]
     mov rsi, 1
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     mov rcx, QWORD PTR [rbp - 40]
     mov rax, rcx
     mov rsp, rbp
     pop rbp
     ret
     jmp .L_IF44
.L_IF43:
.L_IF44:
     movsxd rcx, DWORD PTR [rbp - 32]
     mov rdx, 0
     cmp rcx, rdx
     setb al
     movzx rcx, al
     movsxd rdx, DWORD PTR [rbp - 48]
     mov rsi, 10
     cmp rdx, rsi
     sete al
     movzx rdx, al
     cmp rcx, 0
     je .L_AND49
     cmp rdx, 0
     je .L_AND49
     mov rcx, 1 
     jmp .L_AND48
.L_AND49:
     mov rcx, 0
.L_AND48:
     cmp cl, 0
     je .L_IF45
     mov rcx, 1
     mov BYTE PTR [rbp - 24], cl
     movsxd rcx, DWORD PTR [rbp - 32]
     neg rcx
     mov DWORD PTR [rbp - 32], ecx
     jmp .L_IF46
.L_IF45:
.L_IF46:
.L_LOOP49:
     movsxd rcx, DWORD PTR [rbp - 32]
     mov rdx, 0
     cmp rcx, rdx
     seta al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP50
     movsxd rcx, DWORD PTR [rbp - 32]
     movsxd rdx, DWORD PTR [rbp - 48]
     push rax
     push rbx
     push rdx
     mov rax, rcx
     mov rbx, rdx
     mov edx, 0
     div rbx
     mov rcx, rdx
     pop rdx
     pop rbx
     pop rax
     mov DWORD PTR [rbp - 8], ecx
     movsxd rcx, DWORD PTR [rbp - 8]
     mov rdx, 10
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_IF51
     mov rcx, 48
     movsxd rdx, DWORD PTR [rbp - 8]
     add rcx, rdx
     mov rdx, QWORD PTR [rbp - 40]
     movsxd rsi, DWORD PTR [rbp - 16]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     jmp .L_IF52
.L_IF51:
     mov rcx, 97
     movsxd rdx, DWORD PTR [rbp - 8]
     mov rsi, 10
     sub rdx, rsi
     add rcx, rdx
     mov rdx, QWORD PTR [rbp - 40]
     movsxd rsi, DWORD PTR [rbp - 16]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
.L_IF52:
     movsxd rcx, DWORD PTR [rbp - 16]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 16], ecx
     movsxd rcx, DWORD PTR [rbp - 32]
     movsxd rdx, DWORD PTR [rbp - 48]
     push rax
     push rbx
     push rdx
     mov rax, rcx
     mov rbx, rdx
     mov edx, 0
     div rbx
     mov rcx, rax
     pop rdx
     pop rbx
     pop rax
     mov DWORD PTR [rbp - 32], ecx
     jmp .L_LOOP49
.L_LOOP50:
     movzx rcx, BYTE PTR [rbp - 24]
     cmp rcx, 0
     je .L_IF53
     mov rcx, 45
     mov rdx, QWORD PTR [rbp - 40]
     movsxd rsi, DWORD PTR [rbp - 16]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     movsxd rcx, DWORD PTR [rbp - 16]
     mov rdx, 1
     add rcx, rdx
     mov DWORD PTR [rbp - 16], ecx
     jmp .L_IF54
.L_IF53:
.L_IF54:
     mov rcx, 0
     mov rdx, QWORD PTR [rbp - 40]
     movsxd rsi, DWORD PTR [rbp - 16]
     add rdx, rsi
     mov BYTE PTR [rdx], cl
     movsxd r10, DWORD PTR [rbp - 16]
     mov rsi, r10
     mov r10, QWORD PTR [rbp - 40]
     mov rdi, r10
     call reverse
     mov rcx, QWORD PTR [rbp - 40]
     mov rax, rcx
     mov rsp, rbp
     pop rbp
     ret
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
reverse:
     push rbp
     mov rbp, rsp
     sub rsp, 48
     mov QWORD PTR [rbp - 40], rdi
     mov QWORD PTR [rbp - 48], rsi
     xor rax, rax
     mov BYTE PTR [rbp - 8], al
     mov DWORD PTR [rbp - 16], eax
     mov BYTE PTR [rbp - 24], al
     mov DWORD PTR [rbp - 32], eax
     mov rcx, 0
     mov DWORD PTR [rbp - 16], ecx
     movsxd rcx, DWORD PTR [rbp - 48]
     mov rdx, 1
     sub rcx, rdx
     mov DWORD PTR [rbp - 32], ecx
.L_LOOP55:
     movsxd rcx, DWORD PTR [rbp - 16]
     movsxd rdx, DWORD PTR [rbp - 32]
     cmp rcx, rdx
     setb al
     movzx rcx, al
     cmp cl, 0
     je .L_LOOP56
     mov rdx, QWORD PTR [rbp - 40]
     movsxd rsi, DWORD PTR [rbp - 16]
     add rdx, rsi
     mov rsi, QWORD PTR [rbp - 40]
     movsxd rdi, DWORD PTR [rbp - 16]
     add rsi, rdi
     xor rcx, rcx
     mov cl, BYTE PTR [rsi]
     mov BYTE PTR [rbp - 8], cl
     mov rsi, QWORD PTR [rbp - 40]
     movsxd rdi, DWORD PTR [rbp - 32]
     add rsi, rdi
     mov rdi, QWORD PTR [rbp - 40]
     movsxd r8, DWORD PTR [rbp - 32]
     add rdi, r8
     xor rcx, rcx
     mov cl, BYTE PTR [rdi]
     mov BYTE PTR [rbp - 24], cl
     movzx rcx, BYTE PTR [rbp - 24]
     mov rdi, QWORD PTR [rbp - 40]
     movsxd r8, DWORD PTR [rbp - 16]
     add rdi, r8
     mov BYTE PTR [rdi], cl
     movzx rcx, BYTE PTR [rbp - 8]
     mov rdi, QWORD PTR [rbp - 40]
     movsxd r8, DWORD PTR [rbp - 32]
     add rdi, r8
     mov BYTE PTR [rdi], cl
     movsxd rcx, DWORD PTR [rbp - 16]
     mov rdi, 1
     add rcx, rdi
     mov DWORD PTR [rbp - 16], ecx
     movsxd rcx, DWORD PTR [rbp - 32]
     mov rdi, 1
     sub rcx, rdi
     mov DWORD PTR [rbp - 32], ecx
     jmp .L_LOOP55
.L_LOOP56:
     xor rax, rax
     mov rsp, rbp
     pop rbp     
     ret         
