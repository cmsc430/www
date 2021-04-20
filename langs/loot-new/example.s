        global entry
        default rel
        section .text
        extern peek_byte
        extern read_byte
        extern write_byte
        extern raise_error
entry:
        mov rbx, rdi
        sub rsp, 8
        mov rax, 16
        push rax
        mov rax, 32
        push rax
        mov rax, 48
        push rax
        mov rax, 64
        push rax
        call label_f_5e96933745
        add rsp, 40
        mov rdx, rbx
        ret
label_double_6334fa372629b92:
        mov rax, [rsp + 8]
        push rax
        mov rax, [rsp + 16]
        pop r8
        mov r9, r8
        and r9, 15
        cmp r9, 0
        jne raise_error
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne raise_error
        add rax, r8
        push rax
        mov rax, [rsp + 24]
        push rax
        mov rax, [rsp + 32]
        pop r8
        mov r9, r8
        and r9, 15
        cmp r9, 0
        jne raise_error
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne raise_error
        add rax, r8
        pop r8
        mov r9, r8
        and r9, 15
        cmp r9, 0
        jne raise_error
        mov r9, rax
        and r9, 15
        cmp r9, 0
        jne raise_error
        add rax, r8
        ret
label_f_5e96933745:
        lea rax, [rel label_double_6334fa372629b92]
        mov [rbx + 0], rax
        mov rax, rbx
        or rax, 4
        add rbx, 8
        mov r9, rax
        and r9, 7
        cmp r9, 4
        jne raise_error
        push rax
        mov rax, 32
        push rax
        mov rax, 32
        push rax
        mov r9, [rsp + 8]
        mov [rsp + 40], r9
        mov r9, [rsp + 0]
        mov [rsp + 32], r9
        mov rax, [rsp + 16]
        xor rax, 4
        add rsp, 24
        jmp [rax + 0]
        ret

