#lang racket

(provide (all-defined-out))

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

; Integer -> Integer -> Integer
; Implement addition between two integers the way x64 does.
(define x64-add (curry twos-complement-add 64))

; Integer -> Integer -> Integer
; Implement multiplication between two integers the way x64 does.
(define x64-mul (curry twos-complement-mul 64))

(module+ test
  (require rackunit racket/unsafe/ops)

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

; Integer -> Integer
; The a function that takes an integer, and produces the integer with the same system
; call with macOS BSD system call table prefix.
(define macos-prefix (curry + #x2000000))

; The value of the exit system call, as a decimal integer.
(define sys-exit
  (match (system-type)
    ['unix 60]
    ['macosx (macos-prefix #x1)]
    [_ (error "Tried to generate a syscall for Windows, which ought to be impossible.")]))

; String
; The name of the label that the linker expects as the starting block.
(define start-label "start")

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
(define (compile e)
  (when (null? (current-pass-list))
    (error 'compile "Did you remember to initialize current-pass-list?"))

  ((apply compose (reverse (current-pass-list))) e))

; A string representing the binary file type for this operating system, suitable for use with nasm -f
(define bin-format
  (match (system-type)
    ['unix "elf64"]
    ['macosx "macho64"]
    [_ (error "Unsupported operating system")]))

; A string representing additional ld flags for this operating system.
(define ld-flags
  (match (system-type)
    ['macosx "-macosx_version_min 10.6 -e start"]
    [_ "-e start"]))

; L1 -> integer
; Compiles the L1 program e using the compiler induced by (current-pass-list),
; executes it natively, and returns the integer given as the exit code.
; Expects nasm and ld to be in the path.
(define (execute e)
  (define p (path->string (make-temporary-file "rkt~a.s")))
  (define o (string-replace p ".s" ".o"))
  (define exe (string-replace p ".s" ".exe"))

  (define asm (compile e))

  (with-output-to-file p (thunk (display asm)) #:exists 'replace)

  (unless (zero? (system/exit-code (format "nasm -f ~a ~a -o ~a" bin-format p o)))
    (with-input-from-file p (thunk (displayln (port->string))))
    (error 'execute "Failed to compile"))

  (unless (zero? (system/exit-code (format "ld ~a -o ~a ~a" ld-flags exe o)))
    (displayln (format "ld ~a -o ~a ~a" ld-flags exe o))
    (error 'execute "Failed to link"))
  (define res (system/exit-code exe))

  (for ([f (list p o exe)])
    (and (file-exists? f) (delete-file f)))
  res)


(define (TODO str)
  (error str))

(define (CHALLENGE str)
  (displayln str))
