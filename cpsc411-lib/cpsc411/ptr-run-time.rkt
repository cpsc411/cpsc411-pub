#lang at-exp racket/base

(require
 "compiler-lib.rkt"
 (only-in racket/format ~a))

(provide
 (all-defined-out))

(define runtime-error-string "Run-time error; see exit code")

; definitions used by the runtime
; TODO: Should make this a separate file
; TODO: Should package these up a bit.... ORrr, better yet, rewrite them in one
; of the ILs and generate them :D
(define (x86-64-runtime)
  @~a{
; Convert RAX to a string, left in buffer msg
; The result should be a number in RAX
; build the string backwards, then reverse
done:
  mov rax, @(current-return-value-register)
  ; exit after printing.
  mov rsp, stack
  add rsp, @(current-stack-size)
  mov r13, exit
printer:
  mov r10, rax
  ; if fixnum
  and r10, @(current-fixnum-mask)
  cmp r10, @(current-fixnum-tag)
  je fixnum_to_string
  ; if pair
  mov r10, rax
  and r10, @(current-pair-mask)
  cmp r10, @(current-pair-tag)
  je print_pair
  ;  ; if vector
  mov r10, rax
  and r10, @(current-vector-mask)
  cmp r10, @(current-vector-tag)
  je print_vector
  ; if procedure
  mov r10, rax
  and r10, @(current-procedure-mask)
  cmp r10, @(current-procedure-tag)
  je print_procedure
  ; if boolean
  mov r10, rax
  and r10, @(current-boolean-mask)
  cmp r10, @(current-boolean-tag)
  je boolean_to_string
  ; if void
  mov r10, rax
  and r10, @(current-void-mask)
  cmp r10, @(current-void-tag)
  je void_to_string
  ; if empty
  mov r10, rax
  and r10, @(current-empty-mask)
  cmp r10, @(current-empty-tag)
  je empty_to_string
  ; if ascii
  mov r10, rax
  and r10, @(current-ascii-char-mask)
  cmp r10, @(current-ascii-char-tag)
  je ascii_to_string
  ; if error
  mov r10, rax
  and r10, @(current-error-mask)
  cmp r10, @(current-error-tag)
  je error_to_string

invalid_to_string:
  mov rsi, invalid_msg
  mov rdx, invalid_len
  jmp print_msg

print_vector:
  mov r10, rax
  mov rsi, scratch
  ; print left-paren
  mov BYTE [rsi], '#'
  mov BYTE [rsi + 1], '('
  mov rdx, 2
  mov rax, @|(sys-write)|
  mov rdi, 1
  syscall
  ; remove ptr tag
  sub r10, @(current-vector-tag)
  ; load length
  mov r9, [r10 + @(current-vector-length-displacement)]
  ; move pointer to base of payload
  add r10, @(current-vector-base-displacement)
  ; convert length fixnum to int64
  sar r9, @(current-fixnum-shift)
  mov r8, 0
  cmp r9, r8
  je finish_vector
.loop:
  ; print r8th element
  mov rax, [r10 + r8*@|(current-word-size-bytes)|]
  push r13
  push r10
  push r8
  push r9
  mov r13, .loop_return
  jmp printer
.loop_return:
  pop r9
  pop r8
  pop r10
  pop r13
  inc r8
  cmp r9, r8
  je finish_vector
  mov rsi, scratch
  mov BYTE [rsi], ' '
  mov rdx, 1
  mov rax, @|(sys-write)|
  mov rdi, 1
  syscall
  jmp .loop
finish_vector:
  mov rsi, scratch
  mov BYTE [rsi], ')'
  mov rdx, 1
  mov rax, @|(sys-write)|
  mov rdi, 1
  syscall
  jmp r13

print_procedure:
  mov rsi, procedure_msg
  mov rdx, procedure_len
  jmp print_msg

print_pair:
  mov r10, rax
  mov rsi, scratch
  ; print left-paren
  mov BYTE [rsi], '('
  mov rdx, 1
  mov rax, @|(sys-write)|
  mov rdi, 1
  syscall
  ; print first element
  push r13
  push r10
  mov r13, print_second_element
  mov rax, [r10 + @(car-offset)]
  jmp printer
print_second_element:
  pop r10
  pop r13
  mov rsi, scratch
  ; print the . part of a pair
  mov BYTE [rsi], ' '
  mov BYTE [rsi + 1], '.'
  mov BYTE [rsi + 2], ' '
  mov rdx, 3
  mov rax, @|(sys-write)|
  mov rdi, 1
  syscall
  mov rax, [r10 + @(cdr-offset)]
  push r13
  mov r13, print_final_paren
  jmp printer
print_final_paren:
  pop r13
  mov rsi, scratch
  mov BYTE [rsi], ')'
  mov rdx, 1
  mov rax, @|sys-write|
  mov rdi, 1
  syscall
  jmp r13

error_to_string:
  sar rax, @(current-error-shift)
  mov     r10, rax
  mov     rsi, error_msg
  mov     rdx, error_len
  mov     rax, @|(sys-write)|
  mov     rdi, 2
  syscall

  mov     rax, @|(sys-exit)|
  mov     rdi, r10
  syscall

boolean_to_string:
  xor rax, @(current-boolean-tag)
  cmp rax, 0
  je false_to_string
  mov rsi, true_msg
  mov rdx, true_len
  jmp print_msg

false_to_string:
  mov rsi, false_msg
  mov rdx, false_len
  jmp print_msg

void_to_string:
  mov rsi, void_msg
  mov rdx, void_len
  jmp print_msg

empty_to_string:
  mov rsi, empty_msg
  mov rdx, empty_len
  jmp print_msg

ascii_to_string:
  sar rax, @(current-ascii-char-shift)
  mov rsi, ascii_char_msg
  mov rdx, rax
  mov BYTE [rsi + 2], dl
  mov rdx, ascii_char_len
  jmp print_msg

fixnum_to_string:
  sar rax, @(current-fixnum-shift)
  mov rdi, 0                    ; index into msg, starting at beginning
  mov r12, 10                   ; divide by 10; idiv requires register arg
  mov rsi, fixnum_msg
  mov r15, 0                    ; clear r15 to store negative flag
  cmp rax, 0                    ; if negative
  js neg

loop:
  mov rdx, 0                    ; extend rax to rdx
  idiv r12                      ; signed divide RDX:RAX by r12, with result
                                ; stored in RAX ← Quotient, RDX ← Remainder.
  add rdx, 48                   ; convert digit to ASCII char
  mov BYTE [rsi + rdi], dl      ; mov char into msg
  inc rdi
  cmp rax, 0
  jne loop
  cmp r15, 0                    ; if number if negative, add - as final character
  jl add_minus

; rdi contains the length of the msg
; msg is in rsi
reverse_msg:
  mov rdx, rdi ; preserve the length for printing
  dec rdi      ; length -> final index
  mov r9, 0    ; first character
rev_loop:
  cmp rdi, r9
  jle print_msg
  ; Until rdi <= r9, swap [rsi + rdi] and [rsi + r9]
  ; Save last character into register, move first character
  mov r8b, BYTE [rsi + rdi]
  mov r10b, BYTE [rsi + r9]
  mov BYTE [rsi + rdi], r10b
  mov BYTE [rsi + r9], r8b
  inc r9
  dec rdi
  jmp rev_loop

print_msg:
  mov     rax, @|(sys-write)|
  mov     rdi, 1                ; And I want it to write to stdout
                                ; The message pointer is in rsi
                                ; length in rdx
  syscall
  jmp r13

exit:
  mov     rax, @|(sys-exit)|      ; I'm about to call the OS sys_exit function
  mov     rdi, 0                ; The exit code is 0
  syscall

neg:
  mov r15, -1
  imul rax, -1
  jmp loop

add_minus:
  mov BYTE [rsi + rdi], 45
  inc rdi
  jmp reverse_msg})

;; TODO: Some abstract for the printed representation of these things?
; statically allocated data used by the runtime
(define (x86-64-runtime-data)
  @~a{
invalid_msg: db 'Invalid data returned'
invalid_len: equ $-invalid_msg

true_msg: db '#t'
true_len: equ $-true_msg

false_msg: db '#f'
false_len: equ $-false_msg

empty_msg: db '()'
empty_len: equ $-empty_msg

void_msg: db ''
void_len: equ $-void_msg

ascii_char_msg: db '#\ '
ascii_char_len: equ $-ascii_char_msg

error_msg: db '@|runtime-error-string|'
error_len: equ $-error_msg

procedure_msg: db '#<procedure>'
procedure_len: equ $-procedure_msg
})

; statically allocated uninitialized data used by the runtime
; NOTE: Must be a function to get binding time of parameters right.
(define (x86-64-uninitialized-data)
  @~a{stack:   resb @(current-stack-size)
      scratch: resb 8
      fixnum_msg:   resb 19})

; TODO should separate boilerplate and run-time
(define wrap-x64-run-time values)

; TODO: This stack is stupid and it should just start at stack and incremenet from there.
; Remove (rbp - offset) form.
(define (wrap-x64-boilerplate e)
  @~a{
global @|start-label|

section .text

@|start-label|:
  ; setup heap with mmap
  mov rax, @|(sys-mmap)|
  mov rdi, 0  ; ask OS to find a page-aligned place
  mov rsi, @(current-heap-size) ; mmap this many bytes
  mov rdx, @(bitwise-ior mmap-read mmap-write)  ; permissions: read and write
  mov r10, @(bitwise-ior mmap-private mmap-anonymous) ; type of map: memory
  mov r8, -1   ; -1 file descriptor for no file
  mov r9, 0    ; no offset
  syscall
  mov @(current-heap-base-pointer-register), rax
  mov @(current-frame-base-pointer-register), stack
  ; move pointer to middle stack, to allow operands in both directions.
  add @(current-frame-base-pointer-register), @(/ (current-stack-size) 2)
  mov @(current-return-address-register), done
@|e|
  jmp done

@(x86-64-runtime)

section .bss
@(x86-64-uninitialized-data)

section .data

dummy: db 0 ; Mac isn't happy with an empty .data
@(x86-64-runtime-data)
})

