#lang at-exp racket

(require
 racket/syntax
 "info-lib.rkt"
 "machine-ints.rkt")

(provide
 (all-defined-out)
 ;; TODO: backwards compat
 (rename-out
  [current-auxiliary-registers
   current-patch-instructions-registers])
 (all-from-out "info-lib.rkt")
 (all-from-out "machine-ints.rkt"))

(define current-word-size-bytes (make-parameter 8))
(define current-register-set (make-parameter '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)))

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


(define current-pair-shift (make-parameter 3))
(define current-pair-mask (make-parameter #b111))
(define current-pair-tag (make-parameter #b001))

(define (words->bytes n)
  (* n (current-word-size-bytes)))

(define current-car-displacement
  (make-parameter (words->bytes 0)))
(define current-cdr-displacement
  (make-parameter (words->bytes 1)))
(define current-pair-size
  (make-parameter (words->bytes 2)))

(define (car-offset)
  (- (current-car-displacement) (current-pair-tag)))

(define (cdr-offset)
  (- (current-cdr-displacement) (current-pair-tag)))

(define current-vector-shift (make-parameter 3))
(define current-vector-mask (make-parameter #b111))
(define current-vector-tag (make-parameter #b011))
(define current-vector-length-displacement
  (make-parameter (words->bytes 0)))
(define current-vector-base-displacement
  (make-parameter (words->bytes 1)))

(define current-procedure-shift (make-parameter 3))
(define current-procedure-mask (make-parameter #b111))
(define current-procedure-tag (make-parameter #b010))

(define current-procedure-label-displacement
  (make-parameter (words->bytes 0)))
(define current-procedure-arity-displacement
  (make-parameter (words->bytes 1)))
(define current-procedure-environment-displacement
  (make-parameter (words->bytes 2)))


;; Compiler Helpers
;; ------------------------------------------------------------------------

(define (label? s)
  (and (symbol? s)
       (or (eq? s 'done)
           (regexp-match-exact? #rx"L\\..+\\.[0-9]+" (symbol->string s)))))

(define (allowed-character? c)
  (and (member c (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_#@~.?"))
       #t))

(define (sanitize-label l)
  (string-append*
   (map (λ (c)
          (if (allowed-character? c)
              (string c)
              (format "$~x$" (char->integer c))))
        (string->list (symbol->string l)))))

(define fresh
  (let ([counter (let ([x 0])
                   (lambda ()
                     (set! x (add1 x))
                     x))])
    (lambda ([x 'tmp])
      (format-symbol "~a.~a" (if (label? x) (format-symbol ".~a" x) x)
                     (counter)))))

(define fresh-label
  (let ([counter (let ([x 0])
                   (lambda ()
                     (set! x (add1 x))
                     x))])
    (lambda ([x 'tmp])
      (format-symbol "L.~a.~a" x (counter)))))

(define name? symbol?)

(define (dispoffset? n)
  (and (int32? n)
       (zero? (remainder n 8))))

(define (aloc? s)
  (and (symbol? s)
       (not (register? s))
       (not (label? s))
       (regexp-match-exact? #rx".+\\.[0-9]+" (symbol->string s))))

(define (register? r) (and (memq r (current-register-set)) #t))

;; Expects list of effects and tail
(define (make-begin is tail)
  (let loop ([is (make-begin-effect is)]
             [tail tail])
    (match (cons is tail)
      [(cons '() _) tail]
      [(cons '(begin) _) tail] ; Happens due to interface mismatch between make-begin and make-begin-effect
      [(cons `(begin ,is1 ...)
             `(begin ,is2 ... ,tail))
       (loop (append is1 is2) tail)]
      [(cons `(begin ,is1 ...) tail)
       `(begin ,@is1 ,tail)]
      [(cons is tail)
       `(begin ,@is ,tail)])))

;; Expects non-empty list
(define (make-begin-effect is)
  `(begin
     ,@(let loop ([is is])
         (match is
           ['() '()]
           [`((begin ,e ...) ,e2 ...)
            (append (loop e) (loop e2))]
           [`(,e ,e2 ...)
            (cons e (loop e2))]))))



;; Calling conventions
;; ------------------------------------------------------------------------

(define current-return-value-register
  (make-parameter 'rax))

(define current-return-address-register
  (make-parameter 'r15))

(define current-frame-base-pointer-register
  (make-parameter 'rbp))

(define (frame-base-pointer-register? x)
  (eq? (current-frame-base-pointer-register) x))

(define current-heap-base-pointer-register
  (make-parameter 'r12))

; x64 parameter passing, ish
(define current-parameter-registers
  (make-parameter '(rdi rsi rdx rcx r8 r9)))

; A list containing two registers.
(define current-auxiliary-registers
  (make-parameter '(r10 r11)))

(define current-assignable-registers
  (make-parameter
   (remove*
    (list*
     (current-return-value-register)
     (current-frame-base-pointer-register)
     (current-heap-base-pointer-register)
     (current-auxiliary-registers))
    (current-register-set))))

;; Frame variable abstractions
;; ------------------------------------------------------------------------

(define (fvar? x)
  (and (symbol? x)
       (regexp-match-exact? #rx"^fv[0-9]+" (symbol->string x))))

(define (make-fvar i)
  (format-symbol "fv~a" i))

(define (fvar->index v)
  (string->number (substring (symbol->string v) 2)))

;; Undead analysis
;; ------------------------------------------------------------------------

(define ((_undead-set? loc?) x)
  (and (list? x)
       (andmap loc? x)
       (= (set-count x) (length x))))

(define undead-set? (_undead-set? aloc?))

;; TODO: Parameterize by the language features, so we can provide the v1, v4,
;; and v6 versions.
(define ((_undead-set-tree? loc?) ust)
  (let ([undead-set? (_undead-set? loc?)]
        [undead-set-tree? (_undead-set-tree? loc?)])
    (match ust
      ; for an instructions
      [(? undead-set?) #t]
      ; for a return point
      [(list (? undead-set?) (? undead-set-tree?)) #t]
      ; for an if
      [(list (? undead-set?) (? undead-set-tree?) (? undead-set-tree?)) #t]
      ; for a begin
      [`(,(? undead-set-tree?) ... ,(? undead-set-tree?)) #t]
      [else #f])))

(define undead-set-tree? (_undead-set-tree? aloc?))

(define undead-set-tree/rloc? (_undead-set-tree? (or/c aloc? fvar? register?)))

#;(define undead-set-list? (listof undead-set?))

(define (ascii-char-literal? x)
  (and (char? x) (<= 40 (char->integer x) 176)))

(define (int61? x)
  (int-size? 61 x))

(define (uint8? x)
  (<= 0 x 255))

;; Compiler stuff
;; ------------------------------------------------------------------------

; A string representing the binary file type for this operating system, suitable for use with nasm -f
(define (bin-format [type (system-type)])
  (match type
    ['unix "elf64"]
    ['macosx "macho64"]
    ['windows "win64"]))

; A string representing additional ld flags for this operating system.
(define (ld-flags [type (system-type)])
  (match type
    ['macosx "-macosx_version_min 10.6 -e start"]
    [_ "-e start"]))

; String
; The name of the label that the linker expects as the starting block.
(define start-label "start")

; TODO This abstraction doesn't scale to Windows
(define macos-prefix (curry + #x2000000))

; The value of the write system call, as a decimal integer.
(define (sys-write [type (system-type)])
  (match type
    ['unix 1]
    ['macosx (macos-prefix #x4)]
    [_ (error "This case should never happen; windows is special")]))

; The value of the exit system call, as a decimal integer.
(define (sys-exit [type (system-type)])
  (match type
    ['unix 60]
    ['macosx (macos-prefix #x1)]
    [_ (error "This case should never happen; windows is special")]))

(define (sys-mmap [type (system-type)])
  (match type
    ['unix 9]
    ['macosx (macos-prefix 197)]
    [_ (error "This case should never happen; windows is special")]))

(define mmap-read #x01)
(define mmap-write #x2)
(define mmap-private #x2)
(define mmap-anonymous
  (match (system-type)
    ['unix #x20]
    ['macosx #x1000]
    [_ (error "This case should never happen; windows is special")]))

; A parameter representing the current stack size in bytes
; Default is 8MB, which should be plenty.
;
; Ought to be using mmap.
(define current-stack-size (make-parameter (* 8 1024 1024)))

; A parameter representing the current heap size in bytes
; Default is 128MB, which should be plenty.
(define current-heap-size (make-parameter (* 128 1024 1024)))

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

  (unless (zero? (system/exit-code @~a{ld @|(ld-flags)| -o @|exe| @|o|}))
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
(define (nasm-run/input-read n)
  (nasm-run/observe
   (lambda (x)
     (with-input-from-string
       (with-output-to-string
         (thunk
          (system
           (string-join
            (cons x (build-list (sub1 n) (lambda _ "a")))
            " "))))
       (thunk (read))))))

(define nasm-run/error-string
  (nasm-run/observe (lambda (x)
                      (define op (open-output-string))
                      (parameterize ([current-error-port op])
                        (system x)
                        (get-output-string op)))))

(define nasm-run/error-string+code
  (nasm-run/observe (lambda (x)
                      (define op (open-output-string))
                      (parameterize ([current-error-port op])
                        (let ([code (system/exit-code x)])
                          (cons code (get-output-string op)))))))

; L? -> (x64 String -> Any) -> Any
; Takes an expression from the current language, and an optional nasm-reader,
; compiles and executes the expression using (current-pass-list), then
; assembles, links, and executes the program natively, returning the resulting
; datum as read by the nasm-reader.
(define current-run/read (make-parameter nasm-run/read))
(define (execute x [read (current-run/read)]) ((compose read compile) x))

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

;; ------------------------------------------------------------------------

;; Asm-lang/assignents -> Asm-lang/assignents
;; Assumes the 'conflicts info field is preserved into Asm-lang/assignments
(require racket/contract)
(define/contract (check-assignment p)
  (->
   (cons/c
    any/c
    (cons/c
     (let ([loc? (or/c register? fvar?)])
      (info/c
       (assignment ((aloc? loc?) ...))
       (conflicts ((aloc? (aloc? ...)) ...))
       (locals (aloc? ...))))
     any/c))
   any/c)
  (define who 'check-assignment)
  (define info (second p))
  (define assign
    (make-immutable-hash
     (map (lambda (pair) (cons (first pair) (second pair)))
          (info-ref info 'assignment))))
  (define conflicts
    (make-immutable-hash
     (map (lambda (adj-list) (cons (first adj-list) (second adj-list)))
          (info-ref info 'conflicts))))
  (define locals (info-ref info 'locals))
  (unless (set=? locals (hash-keys assign))
      (error who
             (for/fold ([f "Some locals not assigned homes:~n  homeless locals:"])
                       ([i (set-subtract locals (hash-keys assign))])
               (format "~a ~a" f i))))
  (let ([edges (foldl (lambda (pair result)
                        (let ([node (car pair)]
                              [adj-list (cdr pair)])
                          (foldl (lambda (adj result2)
                                   (set-add result2 (cons node adj)))
                                 result
                                 adj-list)))
                      (set)
                      (hash->list conflicts))])
    (for ([edge edges])
      (let ([u (car edge)]
            [v (cdr edge)])
        (when (equal? (hash-ref assign u)
                      (hash-ref assign v))
          (error who
                 (format "Produced bad assignment:~n  ~a and ~a both assigned to ~a"
                         u v (hash-ref assign u))))))
    p))
