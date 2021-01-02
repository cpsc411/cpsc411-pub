#lang at-exp racket

(require racket/syntax)

(provide (all-defined-out))

;; info library
;; ------------------------------------------------------------------------

;; An /info/ is like an association list, but each key must be mapped to a proper
;; list.
;; That is:
;;   info: ((key value) ...)
;;   als:  ((key . value) ...)
;;
;; This makes info's print better, particularly when the value is itself a list.
;; It uses a little more memory, though.

;; info-field key -> value
(define (info-ref info key [default (lambda ()
                                      (raise
                                       (make-exn:fail
                                        (format "info-ref: no value for key ~a" key)
                                        (current-continuation-marks))))])
  (car (dict-ref info key (lambda ()
                            (list
                             (if (procedure? default)
                                 (default)
                                 default))))))

;; info-field key value -> info-field
(define (info-set info key value)
  (dict-set info key (list value)))

;; info-field key -> info-field
(define (info-remove info key)
  (dict-remove info key))

(module+ test
  (require rackunit)
  (check-equal?
   (info-set '()
             'assignment
             '((x rax) (y rbx)))
   '((assignment ((x rax) (y rbx)))))

  (check-equal?
   (info-set '((locals (x y)))
             'assignment
             '((x rax) (y rbx)))
   '((locals (x y))
     (assignment ((x rax) (y rbx)))))

  (check-equal?
   (info-ref
    '((locals (x y))
      (assignment ((x rax) (y rbx))))
    'locals)
   '(x y))

  (check-equal?
   (info-ref
    '((locals (x y))
      (assignment ((x rax) (y rbx))))
    'assignment)
   '((x rax) (y rbx))))

;; Ptr abstractions
;; ------------------------------------------------------------------------

(define current-fixnum-shift (make-parameter 3))
(define current-fixnum-mask (make-parameter #b111))
(define current-fixnum-tag (make-parameter #b000))

(define current-boolean-shift (make-parameter 3))
(define current-boolean-mask (make-parameter #b11110111))
(define current-boolean-tag (make-parameter #b110))

(define current-true-ptr (make-parameter #b1110))
(define current-false-ptr (make-parameter #b110))

(define current-empty-mask (make-parameter #b11111111))
(define current-empty-tag (make-parameter #b00010110))
(define current-empty-ptr current-empty-tag)

(define current-void-mask (make-parameter #b11111111))
(define current-void-tag (make-parameter #b00011110))
(define current-void-ptr current-void-tag)

(define current-ascii-char-shift (make-parameter 8))
(define current-ascii-char-mask (make-parameter #b11111111))
(define current-ascii-char-tag (make-parameter #b00101110))

(define current-error-shift (make-parameter 8))
(define current-error-mask (make-parameter #b11111111))
(define current-error-tag (make-parameter #b00111110))

;; Compiler Helpers
;; ------------------------------------------------------------------------

; Symbol -> aloc
;        OR
; -> aloc
; Creates a new abstract location from the given symbol.
; When applied without arguments, creates a new temporary aloc.
(define fresh
  (let ([counter (let ([x 0])
                   (lambda ()
                     (set! x (add1 x))
                     x))])
    (lambda ([x 'tmp])
      (format-symbol "~a.~a" x (counter)))))

; Symbol -> label
;        OR
; -> label
; Creates a new label from the given symbol.
; When applied without arguments, creates a new temporary label.
(define fresh-label
  (let ([counter (let ([x 0])
                   (lambda ()
                     (set! x (add1 x))
                     x))])
    (lambda ([x 'tmp])
      (format-symbol "L.~a.~a" x (counter)))))

(define word-size-bytes 8)

(define (max-int word-size) (sub1 (expt 2 (sub1 word-size))))
(define (min-int word-size) (* -1 (expt 2 (sub1 word-size))))

; Integer -> Integer -> Boolean
; Takes an word size, represented as an integer, and an integer i, and returns
; whether i is in the range for two's complement binary integer representation
; using word-size bits.
(define (int-size? word-size i)
  (and (number? i)
       (exact? i)
       (<= (min-int word-size) i (max-int word-size))))

; Integer -> Boolean
; Returns true iff i is in the range for a 32-bit integer.
(define (int32? i) (int-size? 32 i))

; Integer -> Boolean
; Returns true iff i is in the range for a 64-bit integer.
(define (int64? i) (int-size? 64 i))

; Integer -> Integer -> Integer
; Handle the overflow for the integer x, treating it as an integer in word-size two's
; complement representation.
(define (handle-overflow word-size x)
  (let handle-overflow ([x x])
    (cond
      [(int-size? word-size x) x]
      ; Not sure what this is handling; it's from Kent.
      [(not (= x (bitwise-and (sub1 (expt 2 word-size)) x)))
       (handle-overflow (bitwise-and (sub1 (expt 2 word-size)) x))]
      [(< x 0)
       (handle-overflow (+ x (expt 2 word-size)))]
      [else
       (handle-overflow (- x (expt 2 word-size)))])))

; Integer -> Integer -> Integer -> Integer
; Compute the result of adding n1 and n2 as word-size two's complement
; representation binary numbers.
; Requires that n1 and n2 be valid word-sized integers.
(define (twos-complement-add word-size n1 n2)
  (handle-overflow word-size (+ n1 n2)))

; Integer -> Integer -> Integer -> Integer
; Compute the result of multiplying n1 and n2 as word-size two's complement
; representation binary numbers.
; Requires that n1 and n2 be valid word-sized integers.
(define (twos-complement-mul word-size n1 n2)
  (handle-overflow word-size (* n1 n2)))

; Integer -> Integer -> Integer -> Integer
; Compute the result of subtracting n2 from n1 as word-size two's complement
; representation binary numbers.
; Requires that n1 and n2 be valid word-sized integers.
(define (twos-complement-sub word-size n1 n2)
  (handle-overflow word-size (- n1 n2)))

; Integer -> Integer -> Integer
; Implement addition between two integers the way x64 does.
(define x64-add (curry twos-complement-add 64))

; Integer -> Integer -> Integer
; Implement subtraction between two integers the way x64 does.
(define x64-sub (curry twos-complement-sub 64))

; Integer -> Integer -> Integer
; Implement multiplication between two integers the way x64 does.
(define x64-mul (curry twos-complement-mul 64))

(module+ test
  (require racket/unsafe/ops)

  (check-equal?
   (twos-complement-add 63 (min-int 63) -1)
   (unsafe-fx+ (min-int 63) -1))

  (check-equal?
   (twos-complement-add 63 (max-int 63) 1)
   (unsafe-fx+ (max-int 63) 1))

  (check-equal?
   (twos-complement-add 63 (max-int 63) (max-int 63))
   (unsafe-fx+ (max-int 63) (max-int 63)))

  (check-equal?
   (twos-complement-add 63 (min-int 63) (min-int 63))
   (unsafe-fx+ (min-int 63) (min-int 63)))

  (check-equal?
   (twos-complement-add 63 42 42)
   (unsafe-fx+ 42 42))

  (check-equal?
   (twos-complement-mul 63 (min-int 63) -1)
   (unsafe-fx* (min-int 63) -1))

  (check-equal?
   (twos-complement-mul 63 (max-int 63) 1)
   (unsafe-fx* (max-int 63) 1))

  (check-equal?
   (twos-complement-mul 63 (max-int 63) (max-int 63))
   (unsafe-fx* (max-int 63) (max-int 63)))

  (check-equal?
   (twos-complement-mul 63 (min-int 63) (min-int 63))
   (unsafe-fx* (min-int 63) (min-int 63)))

  (check-equal?
   (twos-complement-mul 63 42 42)
   (unsafe-fx* 42 42)))

(define registers '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
(define (register? r) (and (memq r registers) #t))

; A list containing two registers.
(define current-patch-instructions-registers
  (make-parameter '(r10 r11)))

(define current-assignable-registers
  (make-parameter
   (remove*
    (append (current-patch-instructions-registers)
            '(rax rbp))
    registers)))

;; Calling conventions
;; ------------------------------------------------------------------------

(define current-return-value-register
  (make-parameter 'rax))

(define current-return-address-register
  (make-parameter 'r15))

(define current-frame-base-pointer-register
  (make-parameter 'rbp))

; x64 parameter passing, ish
(define current-parameter-registers
  (make-parameter '(rdi rsi rdx rcx r8 r9)))

;; Frame variable abstractions
;; ------------------------------------------------------------------------

;; any -> boolean
;; Returns #y is x is a fvar, and #f otherwise.
(define (fvar? x)
  (and (symbol? x)
       (regexp-match-exact? #rx"^fv[0-9]+" (symbol->string x))))

;; Natural -> fvar?
;; Construct the fvar with index i.
(define (make-fvar i)
  (format-symbol "fv~a" i))

;; fvar -> Natural
;; Return the index component of this fvar.
(define (fvar->index v)
  (string->number (substring (symbol->string v) 2)))

;; Natural -> fvar -> addr
;; Given the current frame base pointer offset, and an fvar, construct the
;; corresponding displacement mode operand.
(define (fvar->addr acc f)
  `(,(current-frame-base-pointer-register)
    +
    ,(- (* (fvar->index f) word-size-bytes) acc)))

;; Compiler stuff
;; ------------------------------------------------------------------------

; A string representing the binary file type for this operating system, suitable for use with nasm -f
(define (bin-format [type (system-type)])
  (match type
    ['unix "elf64"]
    ['macosx "macho64"]
    ['windows "win64"]))

; A string representing additional ld flags for this operating system.
(define ld-flags
  (match (system-type)
    ['macosx "-macosx_version_min 10.6 -e start"]
    [_ "-e start"]))

; String
; The name of the label that the linker expects as the starting block.
(define start-label "start")

; TODO This abstraction doesn't scale to Windows
(define macos-prefix (curry + #x2000000))

; The value of the write system call, as a decimal integer.
(define sys-write
  (match (system-type)
    ['unix 1]
    ['macosx (macos-prefix #x4)]
    [_ (error "This case should never happen; windows is special")]))

; The value of the exit system call, as a decimal integer.
(define sys-exit
  (match (system-type)
    ['unix 60]
    ['macosx (macos-prefix #x1)]
    [_ (error "This case should never happen; windows is special")]))

(define sys-mmap
  (match (system-type)
    ['unix 9]
    ['macosx (macos-prefix 197)]
    [_ (error "This case should never happen; windows is special")]))

(define mmap-read 04)
(define mmap-write 02)

; A parameter representing the current stack size in bytes
; Default is 8MB, which should be plenty.
;
; Note that due to how the stack is allocated by our run-time system, increasing
; this will significantly slow down compile time, and increase the size of the
; binary.
; Ought to be using mmap.
(define current-stack-size (make-parameter (* 8 1024 1024)))

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
printer:
  mov r10, rax
  ; if fixnum
  and r10, @(current-fixnum-mask)
  cmp r10, @(current-fixnum-tag)
  je fixnum_to_string
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

error_to_string:
  sar rax, @(current-error-shift)
  mov     r10, rax
  mov     rsi, error_msg
  mov     rdx, error_len
  mov     rax, @|sys-write|
  mov     rdi, 2
  syscall

  mov     rax, @|sys-exit|
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
  mov     rax, @|sys-write|
  mov     rdi, 1                ; And I want it to write to stdout
                                ; The message pointer is in rsi
                                ; length in rdx
  syscall

  mov     rax, @|sys-exit|      ; I'm about to call the OS sys_exit function
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
invalid_msg: db '"Invalid data returned"'
invalid_len: equ $-invalid_msg

fixnum_len:   equ  19
fixnum_msg:   times fixnum_len db '0'

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

error_msg: db 'Run-time error; see exit code'
error_len: equ $-error_msg})

; statically allocated uninitialized data used by the runtime
; NOTE: Must be a function to get binding time of parameters right.
(define (x86-64-uninitialized-data)
  @~a{stack:   resb @(current-stack-size)})

; TODO should separate boilerplate and run-time
(define wrap-x64-run-time values)

; TODO: This stack is stupid and it should just start at stack and incremenet from there.
; Remove (rbp - offset) form.
(define (wrap-x64-boilerplate e)
  @~a{
global @|start-label|

section .text

@|start-label|:
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

; Parameter (List-of Procedures)
; The list of compiler passes to use.
; Example usage:
#|
(begin
  (current-pass-list
   (list
    check-paren-x64
    generate-x64
    wrap-x64-run-time
    wrap-x64-boilerplate))
  (displayln (compile '(begin (set! rax 42))))
  (check-equal?
   (execute '(begin (set! rax 42)))
   (interp-paren-x64 '(begin (set! rax 42)))))

(parameterize ([current-pass-list
                (list
                 check-paren-x64
                 interp-paren-x64)])
  (compile '(begin (set! rax 42))))
|#
(define current-pass-list
  (make-parameter
   '()
   (lambda (ls)
     (unless (map procedure? ls)
       (error 'current-pass-list "Expected a list of compiler passes (functions); did you remember to initialize current-pass-list?" ls))
     ls)))

; L1 -> L2
; Where L1 is the input to the first function in (current-pass-list) and L2 is
; the output language of the last function in (current-pass-list).
; NOTE: Conflicts Racket's compile. Could cause problems.
(define (compile e)
  (when (null? (current-pass-list))
    (error 'compile "Did you remember to initialize current-pass-list?"))

  ((apply compose (reverse (current-pass-list))) e))

; (Path -> any) -> x64 String -> any
; Assembles and links the x64 program represented by the string str using nasm,
; and executes the runner function.
; The runner takes a path to the executable, and should return some Racket value
; representing the output of the executable.
; Expects nasm and ld to be in the path.
(define ((nasm-run/observe runner) str)
  (define p (path->string (make-temporary-file "rkt~a.s")))
  (define o (string-replace p ".s" ".o"))
  (define exe (string-replace p ".s" ".exe"))

  (with-output-to-file p (thunk (display str)) #:exists 'replace)

  ;; TODO: Should probably clean up temporary files on error.
  ;; but I use them for debugging.
  (unless (zero? (system/exit-code @~a{nasm -f @(bin-format) @|p| -o @|o|}))
    (with-input-from-file p (thunk (displayln (port->string))))
    (error 'execute "Failed to compile"))

  (unless (zero? (system/exit-code @~a{ld @|ld-flags| -o @|exe| @|o|}))
    (error 'execute "Failed to link"))

  (define res (runner exe))

  ; delete temporary files
  (for ([f (list p o exe)])
    (and (file-exists? f) (delete-file f)))

  res)

; x64 String -> Integer
; Returns the exit code resulting from assembling, linking, and natively executing the x64 input.
(define nasm-run/exit-code
  (nasm-run/observe system/exit-code))

; x64 String -> String
; Returns the string output resulting from assembling, linking, and natively executing the x64 input.
(define nasm-run/print-string
  (nasm-run/observe (lambda (x) (with-output-to-string (thunk (system x))))))

; x64 String -> Integer
; Returns the integer printed by the program resulting from assembling, linking,
; and natively executing the x64 input.
(define nasm-run/print-number
  (nasm-run/observe (lambda (x) (string->number (with-output-to-string (thunk (system x)))))))

; x64 String -> Any
; Returns the read-able Racket datum printed by the program resulting from assembling, linking,
; and natively executing the x64 input.
(define nasm-run/read
  (nasm-run/observe (lambda (x) (with-input-from-string (with-output-to-string (thunk (system x)))
                                  (thunk (read))))))

; L? -> (x64 String -> Any) -> Any
; Takes an expression from the current language, and an optional nasm-reader,
; compiles and executes the expression using (current-pass-list), then
; assembles, links, and executes the program natively, returning the resulting
; datum as read by the nasm-reader.
(define (execute x [read nasm-run/read]) ((compose read compile) x))

;; For code skeletons
;; ------------------------------------------------------------------------

(define (TODO str)
  (error str))

(define (CHALLENGE str)
  (displayln str))

;; Pretty printing
(require racket/pretty)
(pretty-print-current-style-table
 (pretty-print-extend-style-table
  (pretty-print-current-style-table)
  '(module return-point) '(begin lambda)))


;; tracing
(require racket/trace)
(define current-prior-pass-lists (make-parameter '(())))

(define (trace-compiler!)
  (current-prior-pass-lists (cons (current-pass-list) (current-prior-pass-lists)))
  (current-pass-list
   (for/list ([f (current-pass-list)])
     (procedure-rename
      (eval
       #`(trace-lambda
         #:name #,(object-name f) args (apply #,f args)))
      (object-name f)))))

(define (untrace-compiler!)
  (current-pass-list (car (current-prior-pass-lists)))
  (current-prior-pass-lists (cdr (current-prior-pass-lists))))

(define-syntax-rule (with-traced e)
  (begin
    (trace-compiler!)
    (let ([x e])
      (untrace-compiler!)
      x)))
