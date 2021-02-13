#lang at-exp racket/base

(require
 "compiler-lib.rkt"
 (only-in racket/format ~a))

(provide
 (all-defined-out))

; definitions used by the runtime
; TODO: Should package these up a bit.... ORrr, better yet, rewrite them in one
; of the ILs and generate them :D
(define x86-64-runtime
  @~a{
; Convert RAX to a string, left in buffer msg
; The result should be a number in RAX
; build the string backwards, then reverse
done:
number_to_string:
  mov rdi, 0                   ; index into msg, starting at beginning
  mov r12, 10                   ; divide by 10; idiv requires register arg
  mov rsi, msg
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

  mov     rax, @|(sys-exit)|    ; I'm about to call the OS sys_exit function
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

; statically allocated data used by the runtime
(define x86-64-runtime-data
  @~a{
len:   equ  19
msg:   times len db '0'})

; statically allocated uninitialized data used by the runtime
(define x86-64-uninitialized-data
  @~a{})

; TODO should separate boilerplate and run-time
(define wrap-x64-run-time values)

(define (wrap-x64-boilerplate e)
  @~a{
global @|start-label|

section .text

@|start-label|:
  mov @(current-frame-base-pointer-register), rsp
@|e|
  ; The result should be a number in RAX
  jmp done

@|x86-64-runtime|

section .bss
@|x86-64-uninitialized-data|

section .data

dummy: db 0 ; Mac isn't happy with an empty .data
@|x86-64-runtime-data|
})
