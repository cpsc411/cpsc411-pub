#lang racket/base
(require racket/match)

(provide (all-defined-out))

;; generated from the reference solution
#;(for/list ([pass (drop (drop-right (current-pass-list) 2) 2)])
    (for/list ([i values-lang-v5-programs])
      ((apply compose (reverse (takef
                                (current-pass-list)
                                (lambda (x) (not (eq?
                                                  x
                                                  pass))))))
       i)))
(match-define
  (list
   sequentialize-let-source-progs
   impose-calling-conventions-source-progs
   canonicalize-bind-source-progs
   select-instructions-source-progs
   uncover-locals-source-progs
   undead-analysis-source-progs
   conflict-analysis-source-progs
   assign-registers-source-progs
   replace-locations-source-progs
   optimize-predicates-source-progs
   expose-basic-blocks-source-progs
   resolve-predicates-source-progs
   flatten-program-source-progs
   patch-instructions-source-progs
   implement-fvars-source-progs
   generate-x64-source-progs)

  '(((module (define L.id.1 (lambda (x.1) x.1)) (call L.id.1 5))
   (module
     (define L.odd?.2
       (lambda (x.2)
         (if (= x.2 0) 0 (let ((y.3 (+ x.2 -1))) (call L.even?.3 y.3)))))
     (define L.even?.3
       (lambda (x.4)
         (if (= x.4 0) 1 (let ((y.5 (+ x.4 -1))) (call L.odd?.2 y.5)))))
     (call L.even?.3 5))
   (module (define L.zero.4 (lambda (v0.9 v1.8 v2.7 v3.6) 0)) 0)
   (module
     (define L.id.5 (lambda (x.10) x.10))
     (let ((y.11 L.id.5)) (call y.11 5)))
   (module
     (define L.id1.6 (lambda (x.12) x.12))
     (define L.id2.7 (lambda (x.13) x.13))
     (let ((y.14 (if (true) L.id1.6 L.id2.7))) (call y.14 5))))
  ((module (define L.id.8 (lambda (x.15) x.15)) (call L.id.8 5))
   (module
     (define L.odd?.9
       (lambda (x.16)
         (if (= x.16 0)
           0
           (begin (set! y.17 (+ x.16 -1)) (call L.even?.10 y.17)))))
     (define L.even?.10
       (lambda (x.18)
         (if (= x.18 0)
           1
           (begin (set! y.19 (+ x.18 -1)) (call L.odd?.9 y.19)))))
     (call L.even?.10 5))
   (module (define L.zero.11 (lambda (v0.23 v1.22 v2.21 v3.20) 0)) 0)
   (module
     (define L.id.12 (lambda (x.24) x.24))
     (begin (set! y.25 L.id.12) (call y.25 5)))
   (module
     (define L.id1.13 (lambda (x.26) x.26))
     (define L.id2.14 (lambda (x.27) x.27))
     (begin (set! y.28 (if (true) L.id1.13 L.id2.14)) (call y.28 5))))
  ((module
     (define L.id.15 (begin (set! x.29 rdi) x.29))
     (begin (set! rdi 5) (jump L.id.15 rbp rdi)))
   (module
     (define L.odd?.16
       (begin
         (set! x.30 rdi)
         (if (= x.30 0)
           0
           (begin
             (set! y.31 (+ x.30 -1))
             (begin (set! rdi y.31) (jump L.even?.17 rbp rdi))))))
     (define L.even?.17
       (begin
         (set! x.32 rdi)
         (if (= x.32 0)
           1
           (begin
             (set! y.33 (+ x.32 -1))
             (begin (set! rdi y.33) (jump L.odd?.16 rbp rdi))))))
     (begin (set! rdi 5) (jump L.even?.17 rbp rdi)))
   (module
     (define L.zero.18
       (begin
         (set! v0.37 rdi)
         (set! v1.36 rsi)
         (set! v2.35 rdx)
         (set! v3.34 rcx)
         0))
     0)
   (module
     (define L.id.19 (begin (set! x.38 rdi) x.38))
     (begin (set! y.39 L.id.19) (begin (set! rdi 5) (jump y.39 rbp rdi))))
   (module
     (define L.id1.20 (begin (set! x.40 rdi) x.40))
     (define L.id2.21 (begin (set! x.41 rdi) x.41))
     (begin
       (set! y.42 (if (true) L.id1.20 L.id2.21))
       (begin (set! rdi 5) (jump y.42 rbp rdi)))))
  ((module
     (define L.id.22 (begin (set! x.43 rdi) x.43))
     (begin (set! rdi 5) (jump L.id.22 rbp rdi)))
   (module
     (define L.odd?.23
       (begin
         (set! x.44 rdi)
         (if (= x.44 0)
           0
           (begin
             (set! y.45 (+ x.44 -1))
             (begin (set! rdi y.45) (jump L.even?.24 rbp rdi))))))
     (define L.even?.24
       (begin
         (set! x.46 rdi)
         (if (= x.46 0)
           1
           (begin
             (set! y.47 (+ x.46 -1))
             (begin (set! rdi y.47) (jump L.odd?.23 rbp rdi))))))
     (begin (set! rdi 5) (jump L.even?.24 rbp rdi)))
   (module
     (define L.zero.25
       (begin
         (set! v0.51 rdi)
         (set! v1.50 rsi)
         (set! v2.49 rdx)
         (set! v3.48 rcx)
         0))
     0)
   (module
     (define L.id.26 (begin (set! x.52 rdi) x.52))
     (begin (set! y.53 L.id.26) (begin (set! rdi 5) (jump y.53 rbp rdi))))
   (module
     (define L.id1.27 (begin (set! x.54 rdi) x.54))
     (define L.id2.28 (begin (set! x.55 rdi) x.55))
     (begin
       (if (true) (set! y.56 L.id1.27) (set! y.56 L.id2.28))
       (begin (set! rdi 5) (jump y.56 rbp rdi)))))
  ((module
     ()
     (define L.id.29 () (begin (set! x.57 rdi) (halt x.57)))
     (begin (set! rdi 5) (jump L.id.29 rbp rdi)))
   (module
     ()
     (define L.odd?.30
       ()
       (begin
         (set! x.58 rdi)
         (if (= x.58 0)
           (halt 0)
           (begin
             (set! tmp.62 x.58)
             (set! tmp.62 (+ tmp.62 -1))
             (set! y.59 tmp.62)
             (set! rdi y.59)
             (jump L.even?.31 rbp rdi)))))
     (define L.even?.31
       ()
       (begin
         (set! x.60 rdi)
         (if (= x.60 0)
           (halt 1)
           (begin
             (set! tmp.63 x.60)
             (set! tmp.63 (+ tmp.63 -1))
             (set! y.61 tmp.63)
             (set! rdi y.61)
             (jump L.odd?.30 rbp rdi)))))
     (begin (set! rdi 5) (jump L.even?.31 rbp rdi)))
   (module
     ()
     (define L.zero.32
       ()
       (begin
         (set! v0.67 rdi)
         (set! v1.66 rsi)
         (set! v2.65 rdx)
         (set! v3.64 rcx)
         (halt 0)))
     (halt 0))
   (module
     ()
     (define L.id.33 () (begin (set! x.68 rdi) (halt x.68)))
     (begin (set! y.69 L.id.33) (set! rdi 5) (jump y.69 rbp rdi)))
   (module
     ()
     (define L.id1.34 () (begin (set! x.70 rdi) (halt x.70)))
     (define L.id2.35 () (begin (set! x.71 rdi) (halt x.71)))
     (begin
       (if (true) (begin (set! y.72 L.id1.34)) (begin (set! y.72 L.id2.35)))
       (set! rdi 5)
       (jump y.72 rbp rdi))))
  ((module
     ((locals ()))
     (define L.id.36 ((locals (x.73))) (begin (set! x.73 rdi) (halt x.73)))
     (begin (set! rdi 5) (jump L.id.36 rbp rdi)))
   (module
     ((locals ()))
     (define L.odd?.37
       ((locals (tmp.78 y.75 x.74)))
       (begin
         (set! x.74 rdi)
         (if (= x.74 0)
           (halt 0)
           (begin
             (set! tmp.78 x.74)
             (set! tmp.78 (+ tmp.78 -1))
             (set! y.75 tmp.78)
             (set! rdi y.75)
             (jump L.even?.38 rbp rdi)))))
     (define L.even?.38
       ((locals (tmp.79 y.77 x.76)))
       (begin
         (set! x.76 rdi)
         (if (= x.76 0)
           (halt 1)
           (begin
             (set! tmp.79 x.76)
             (set! tmp.79 (+ tmp.79 -1))
             (set! y.77 tmp.79)
             (set! rdi y.77)
             (jump L.odd?.37 rbp rdi)))))
     (begin (set! rdi 5) (jump L.even?.38 rbp rdi)))
   (module
     ((locals ()))
     (define L.zero.39
       ((locals (v3.80 v2.81 v1.82 v0.83)))
       (begin
         (set! v0.83 rdi)
         (set! v1.82 rsi)
         (set! v2.81 rdx)
         (set! v3.80 rcx)
         (halt 0)))
     (halt 0))
   (module
     ((locals (y.85)))
     (define L.id.40 ((locals (x.84))) (begin (set! x.84 rdi) (halt x.84)))
     (begin (set! y.85 L.id.40) (set! rdi 5) (jump y.85 rbp rdi)))
   (module
     ((locals (y.88)))
     (define L.id1.41 ((locals (x.86))) (begin (set! x.86 rdi) (halt x.86)))
     (define L.id2.42 ((locals (x.87))) (begin (set! x.87 rdi) (halt x.87)))
     (begin
       (if (true) (begin (set! y.88 L.id1.41)) (begin (set! y.88 L.id2.42)))
       (set! rdi 5)
       (jump y.88 rbp rdi))))
  ((module
     ((locals ()) (undead-out ((rdi rbp) (rdi rbp))))
     (define L.id.43
       ((locals (x.89)) (undead-out ((x.89) ())))
       (begin (set! x.89 rdi) (halt x.89)))
     (begin (set! rdi 5) (jump L.id.43 rbp rdi)))
   (module
     ((locals ()) (undead-out ((rdi rbp) (rdi rbp))))
     (define L.odd?.44
       ((locals (tmp.94 y.91 x.90))
        (undead-out
         ((rbp x.90)
          ((rbp x.90)
           ()
           ((tmp.94 rbp) (tmp.94 rbp) (y.91 rbp) (rdi rbp) (rdi rbp))))))
       (begin
         (set! x.90 rdi)
         (if (= x.90 0)
           (halt 0)
           (begin
             (set! tmp.94 x.90)
             (set! tmp.94 (+ tmp.94 -1))
             (set! y.91 tmp.94)
             (set! rdi y.91)
             (jump L.even?.45 rbp rdi)))))
     (define L.even?.45
       ((locals (tmp.95 y.93 x.92))
        (undead-out
         ((rbp x.92)
          ((rbp x.92)
           ()
           ((tmp.95 rbp) (tmp.95 rbp) (y.93 rbp) (rdi rbp) (rdi rbp))))))
       (begin
         (set! x.92 rdi)
         (if (= x.92 0)
           (halt 1)
           (begin
             (set! tmp.95 x.92)
             (set! tmp.95 (+ tmp.95 -1))
             (set! y.93 tmp.95)
             (set! rdi y.93)
             (jump L.odd?.44 rbp rdi)))))
     (begin (set! rdi 5) (jump L.even?.45 rbp rdi)))
   (module
     ((locals ()) (undead-out ()))
     (define L.zero.46
       ((locals (v3.96 v2.97 v1.98 v0.99))
        (undead-out ((rsi rdx rcx) (rdx rcx) (rcx) () ())))
       (begin
         (set! v0.99 rdi)
         (set! v1.98 rsi)
         (set! v2.97 rdx)
         (set! v3.96 rcx)
         (halt 0)))
     (halt 0))
   (module
     ((locals (y.101))
      (undead-out ((y.101 rbp) (y.101 rdi rbp) (y.101 rdi rbp))))
     (define L.id.47
       ((locals (x.100)) (undead-out ((x.100) ())))
       (begin (set! x.100 rdi) (halt x.100)))
     (begin (set! y.101 L.id.47) (set! rdi 5) (jump y.101 rbp rdi)))
   (module
     ((locals (y.104))
      (undead-out
       (((rbp) ((y.104 rbp)) ((y.104 rbp))) (y.104 rdi rbp) (y.104 rdi rbp))))
     (define L.id1.48
       ((locals (x.102)) (undead-out ((x.102) ())))
       (begin (set! x.102 rdi) (halt x.102)))
     (define L.id2.49
       ((locals (x.103)) (undead-out ((x.103) ())))
       (begin (set! x.103 rdi) (halt x.103)))
     (begin
       (if (true) (begin (set! y.104 L.id1.48)) (begin (set! y.104 L.id2.49)))
       (set! rdi 5)
       (jump y.104 rbp rdi))))
  ((module
     ((locals ())
      (undead-out ((rdi rbp) (rdi rbp)))
      (conflicts ((rdi (rbp)) (rbp (rdi)))))
     (define L.id.50
       ((locals (x.105)) (undead-out ((x.105) ())) (conflicts ((x.105 ()))))
       (begin (set! x.105 rdi) (halt x.105)))
     (begin (set! rdi 5) (jump L.id.50 rbp rdi)))
   (module
     ((locals ())
      (undead-out ((rdi rbp) (rdi rbp)))
      (conflicts ((rdi (rbp)) (rbp (rdi)))))
     (define L.odd?.51
       ((locals (tmp.110 y.107 x.106))
        (undead-out
         ((rbp x.106)
          ((rbp x.106)
           ()
           ((tmp.110 rbp) (tmp.110 rbp) (y.107 rbp) (rdi rbp) (rdi rbp)))))
        (conflicts
         ((x.106 (rbp))
          (y.107 (rbp))
          (tmp.110 (rbp))
          (rbp (x.106 rdi y.107 tmp.110))
          (rdi (rbp)))))
       (begin
         (set! x.106 rdi)
         (if (= x.106 0)
           (halt 0)
           (begin
             (set! tmp.110 x.106)
             (set! tmp.110 (+ tmp.110 -1))
             (set! y.107 tmp.110)
             (set! rdi y.107)
             (jump L.even?.52 rbp rdi)))))
     (define L.even?.52
       ((locals (tmp.111 y.109 x.108))
        (undead-out
         ((rbp x.108)
          ((rbp x.108)
           ()
           ((tmp.111 rbp) (tmp.111 rbp) (y.109 rbp) (rdi rbp) (rdi rbp)))))
        (conflicts
         ((x.108 (rbp))
          (y.109 (rbp))
          (tmp.111 (rbp))
          (rbp (x.108 rdi y.109 tmp.111))
          (rdi (rbp)))))
       (begin
         (set! x.108 rdi)
         (if (= x.108 0)
           (halt 1)
           (begin
             (set! tmp.111 x.108)
             (set! tmp.111 (+ tmp.111 -1))
             (set! y.109 tmp.111)
             (set! rdi y.109)
             (jump L.odd?.51 rbp rdi)))))
     (begin (set! rdi 5) (jump L.even?.52 rbp rdi)))
   (module
     ((locals ()) (undead-out ()) (conflicts ()))
     (define L.zero.53
       ((locals (v3.112 v2.113 v1.114 v0.115))
        (undead-out ((rsi rdx rcx) (rdx rcx) (rcx) () ()))
        (conflicts
         ((v0.115 (rcx rdx rsi))
          (v1.114 (rcx rdx))
          (v2.113 (rcx))
          (v3.112 ())
          (rsi (v0.115))
          (rdx (v1.114 v0.115))
          (rcx (v2.113 v1.114 v0.115)))))
       (begin
         (set! v0.115 rdi)
         (set! v1.114 rsi)
         (set! v2.113 rdx)
         (set! v3.112 rcx)
         (halt 0)))
     (halt 0))
   (module
     ((locals (y.117))
      (undead-out ((y.117 rbp) (y.117 rdi rbp) (y.117 rdi rbp)))
      (conflicts ((y.117 (rdi rbp)) (rbp (rdi y.117)) (rdi (rbp y.117)))))
     (define L.id.54
       ((locals (x.116)) (undead-out ((x.116) ())) (conflicts ((x.116 ()))))
       (begin (set! x.116 rdi) (halt x.116)))
     (begin (set! y.117 L.id.54) (set! rdi 5) (jump y.117 rbp rdi)))
   (module
     ((locals (y.120))
      (undead-out
       (((rbp) ((y.120 rbp)) ((y.120 rbp))) (y.120 rdi rbp) (y.120 rdi rbp)))
      (conflicts ((y.120 (rdi rbp)) (rbp (rdi y.120)) (rdi (rbp y.120)))))
     (define L.id1.55
       ((locals (x.118)) (undead-out ((x.118) ())) (conflicts ((x.118 ()))))
       (begin (set! x.118 rdi) (halt x.118)))
     (define L.id2.56
       ((locals (x.119)) (undead-out ((x.119) ())) (conflicts ((x.119 ()))))
       (begin (set! x.119 rdi) (halt x.119)))
     (begin
       (if (true) (begin (set! y.120 L.id1.55)) (begin (set! y.120 L.id2.56)))
       (set! rdi 5)
       (jump y.120 rbp rdi))))
  ((module
     ((locals ())
      (undead-out ((rdi rbp) (rdi rbp)))
      (conflicts ((rdi (rbp)) (rbp (rdi))))
      (assignment ()))
     (define L.id.57
       ((locals (x.121))
        (undead-out ((x.121) ()))
        (conflicts ((x.121 ())))
        (assignment ((x.121 r15))))
       (begin (set! x.121 rdi) (halt x.121)))
     (begin (set! rdi 5) (jump L.id.57 rbp rdi)))
   (module
     ((locals ())
      (undead-out ((rdi rbp) (rdi rbp)))
      (conflicts ((rdi (rbp)) (rbp (rdi))))
      (assignment ()))
     (define L.odd?.58
       ((locals (tmp.126 y.123 x.122))
        (undead-out
         ((rbp x.122)
          ((rbp x.122)
           ()
           ((tmp.126 rbp) (tmp.126 rbp) (y.123 rbp) (rdi rbp) (rdi rbp)))))
        (conflicts
         ((x.122 (rbp))
          (y.123 (rbp))
          (tmp.126 (rbp))
          (rbp (x.122 rdi y.123 tmp.126))
          (rdi (rbp))))
        (assignment ((x.122 r15) (y.123 r15) (tmp.126 r15))))
       (begin
         (set! x.122 rdi)
         (if (= x.122 0)
           (halt 0)
           (begin
             (set! tmp.126 x.122)
             (set! tmp.126 (+ tmp.126 -1))
             (set! y.123 tmp.126)
             (set! rdi y.123)
             (jump L.even?.59 rbp rdi)))))
     (define L.even?.59
       ((locals (tmp.127 y.125 x.124))
        (undead-out
         ((rbp x.124)
          ((rbp x.124)
           ()
           ((tmp.127 rbp) (tmp.127 rbp) (y.125 rbp) (rdi rbp) (rdi rbp)))))
        (conflicts
         ((x.124 (rbp))
          (y.125 (rbp))
          (tmp.127 (rbp))
          (rbp (x.124 rdi y.125 tmp.127))
          (rdi (rbp))))
        (assignment ((x.124 r15) (y.125 r15) (tmp.127 r15))))
       (begin
         (set! x.124 rdi)
         (if (= x.124 0)
           (halt 1)
           (begin
             (set! tmp.127 x.124)
             (set! tmp.127 (+ tmp.127 -1))
             (set! y.125 tmp.127)
             (set! rdi y.125)
             (jump L.odd?.58 rbp rdi)))))
     (begin (set! rdi 5) (jump L.even?.59 rbp rdi)))
   (module
     ((locals ()) (undead-out ()) (conflicts ()) (assignment ()))
     (define L.zero.60
       ((locals (v3.128 v2.129 v1.130 v0.131))
        (undead-out ((rsi rdx rcx) (rdx rcx) (rcx) () ()))
        (conflicts
         ((v0.131 (rcx rdx rsi))
          (v1.130 (rcx rdx))
          (v2.129 (rcx))
          (v3.128 ())
          (rsi (v0.131))
          (rdx (v1.130 v0.131))
          (rcx (v2.129 v1.130 v0.131))))
        (assignment ((v0.131 r15) (v1.130 r15) (v2.129 r15) (v3.128 r15))))
       (begin
         (set! v0.131 rdi)
         (set! v1.130 rsi)
         (set! v2.129 rdx)
         (set! v3.128 rcx)
         (halt 0)))
     (halt 0))
   (module
     ((locals (y.133))
      (undead-out ((y.133 rbp) (y.133 rdi rbp) (y.133 rdi rbp)))
      (conflicts ((y.133 (rdi rbp)) (rbp (rdi y.133)) (rdi (rbp y.133))))
      (assignment ((y.133 r15))))
     (define L.id.61
       ((locals (x.132))
        (undead-out ((x.132) ()))
        (conflicts ((x.132 ())))
        (assignment ((x.132 r15))))
       (begin (set! x.132 rdi) (halt x.132)))
     (begin (set! y.133 L.id.61) (set! rdi 5) (jump y.133 rbp rdi)))
   (module
     ((locals (y.136))
      (undead-out
       (((rbp) ((y.136 rbp)) ((y.136 rbp))) (y.136 rdi rbp) (y.136 rdi rbp)))
      (conflicts ((y.136 (rdi rbp)) (rbp (rdi y.136)) (rdi (rbp y.136))))
      (assignment ((y.136 r15))))
     (define L.id1.62
       ((locals (x.134))
        (undead-out ((x.134) ()))
        (conflicts ((x.134 ())))
        (assignment ((x.134 r15))))
       (begin (set! x.134 rdi) (halt x.134)))
     (define L.id2.63
       ((locals (x.135))
        (undead-out ((x.135) ()))
        (conflicts ((x.135 ())))
        (assignment ((x.135 r15))))
       (begin (set! x.135 rdi) (halt x.135)))
     (begin
       (if (true) (begin (set! y.136 L.id1.62)) (begin (set! y.136 L.id2.63)))
       (set! rdi 5)
       (jump y.136 rbp rdi))))
  ((module
     (define L.id.64 (begin (set! r15 rdi) (halt r15)))
     (begin (set! rdi 5) (jump L.id.64)))
   (module
     (define L.odd?.65
       (begin
         (set! r15 rdi)
         (if (= r15 0)
           (halt 0)
           (begin
             (set! r15 r15)
             (set! r15 (+ r15 -1))
             (set! r15 r15)
             (set! rdi r15)
             (jump L.even?.66)))))
     (define L.even?.66
       (begin
         (set! r15 rdi)
         (if (= r15 0)
           (halt 1)
           (begin
             (set! r15 r15)
             (set! r15 (+ r15 -1))
             (set! r15 r15)
             (set! rdi r15)
             (jump L.odd?.65)))))
     (begin (set! rdi 5) (jump L.even?.66)))
   (module
     (define L.zero.67
       (begin
         (set! r15 rdi)
         (set! r15 rsi)
         (set! r15 rdx)
         (set! r15 rcx)
         (halt 0)))
     (halt 0))
   (module
     (define L.id.68 (begin (set! r15 rdi) (halt r15)))
     (begin (set! r15 L.id.68) (set! rdi 5) (jump r15)))
   (module
     (define L.id1.69 (begin (set! r15 rdi) (halt r15)))
     (define L.id2.70 (begin (set! r15 rdi) (halt r15)))
     (begin
       (if (true) (begin (set! r15 L.id1.69)) (begin (set! r15 L.id2.70)))
       (set! rdi 5)
       (jump r15))))
  ((module
     (define L.id.71 (begin (set! r15 rdi) (halt r15)))
     (begin (set! rdi 5) (jump L.id.71)))
   (module
     (define L.odd?.72
       (begin
         (set! r15 rdi)
         (if (= r15 0)
           (halt 0)
           (begin
             (set! r15 r15)
             (set! r15 (+ r15 -1))
             (set! r15 r15)
             (set! rdi r15)
             (jump L.even?.73)))))
     (define L.even?.73
       (begin
         (set! r15 rdi)
         (if (= r15 0)
           (halt 1)
           (begin
             (set! r15 r15)
             (set! r15 (+ r15 -1))
             (set! r15 r15)
             (set! rdi r15)
             (jump L.odd?.72)))))
     (begin (set! rdi 5) (jump L.even?.73)))
   (module
     (define L.zero.74
       (begin
         (set! r15 rdi)
         (set! r15 rsi)
         (set! r15 rdx)
         (set! r15 rcx)
         (halt 0)))
     (halt 0))
   (module
     (define L.id.75 (begin (set! r15 rdi) (halt r15)))
     (begin (set! r15 L.id.75) (set! rdi 5) (jump r15)))
   (module
     (define L.id1.76 (begin (set! r15 rdi) (halt r15)))
     (define L.id2.77 (begin (set! r15 rdi) (halt r15)))
     (begin (begin (set! r15 L.id1.76)) (set! rdi 5) (jump r15))))
  ((module
     (define L.__main.79 (begin (set! rdi 5) (jump L.id.78)))
     (define L.id.78 (begin (set! r15 rdi) (halt r15))))
   (module
     (define L.__main.82 (begin (set! rdi 5) (jump L.even?.81)))
     (define L.odd?.80
       (begin
         (set! r15 rdi)
         (if (= r15 0) (jump L.__nested.83) (jump L.__nested.84))))
     (define L.__nested.83 (halt 0))
     (define L.__nested.84
       (begin
         (set! r15 r15)
         (set! r15 (+ r15 -1))
         (set! r15 r15)
         (set! rdi r15)
         (jump L.even?.81)))
     (define L.even?.81
       (begin
         (set! r15 rdi)
         (if (= r15 0) (jump L.__nested.85) (jump L.__nested.86))))
     (define L.__nested.85 (halt 1))
     (define L.__nested.86
       (begin
         (set! r15 r15)
         (set! r15 (+ r15 -1))
         (set! r15 r15)
         (set! rdi r15)
         (jump L.odd?.80))))
   (module
     (define L.__main.88 (halt 0))
     (define L.zero.87
       (begin
         (set! r15 rdi)
         (set! r15 rsi)
         (set! r15 rdx)
         (set! r15 rcx)
         (halt 0))))
   (module
     (define L.__main.90 (begin (set! r15 L.id.89) (set! rdi 5) (jump r15)))
     (define L.id.89 (begin (set! r15 rdi) (halt r15))))
   (module
     (define L.__main.93 (begin (set! r15 L.id1.91) (set! rdi 5) (jump r15)))
     (define L.id1.91 (begin (set! r15 rdi) (halt r15)))
     (define L.id2.92 (begin (set! r15 rdi) (halt r15)))))
  ((module
     (define L.__main.95 (begin (set! rdi 5) (jump L.id.94)))
     (define L.id.94 (begin (set! r15 rdi) (halt r15))))
   (module
     (define L.__main.98 (begin (set! rdi 5) (jump L.even?.97)))
     (define L.odd?.96
       (begin
         (set! r15 rdi)
         (if (= r15 0) (jump L.__nested.99) (jump L.__nested.100))))
     (define L.__nested.99 (halt 0))
     (define L.__nested.100
       (begin
         (set! r15 r15)
         (set! r15 (+ r15 -1))
         (set! r15 r15)
         (set! rdi r15)
         (jump L.even?.97)))
     (define L.even?.97
       (begin
         (set! r15 rdi)
         (if (= r15 0) (jump L.__nested.101) (jump L.__nested.102))))
     (define L.__nested.101 (halt 1))
     (define L.__nested.102
       (begin
         (set! r15 r15)
         (set! r15 (+ r15 -1))
         (set! r15 r15)
         (set! rdi r15)
         (jump L.odd?.96))))
   (module
     (define L.__main.104 (halt 0))
     (define L.zero.103
       (begin
         (set! r15 rdi)
         (set! r15 rsi)
         (set! r15 rdx)
         (set! r15 rcx)
         (halt 0))))
   (module
     (define L.__main.106 (begin (set! r15 L.id.105) (set! rdi 5) (jump r15)))
     (define L.id.105 (begin (set! r15 rdi) (halt r15))))
   (module
     (define L.__main.109 (begin (set! r15 L.id1.107) (set! rdi 5) (jump r15)))
     (define L.id1.107 (begin (set! r15 rdi) (halt r15)))
     (define L.id2.108 (begin (set! r15 rdi) (halt r15)))))
  ((begin
     (set! rdi 5)
     (jump L.id.110)
     (with-label L.id.110 (set! r15 rdi))
     (halt r15))
   (begin
     (set! rdi 5)
     (jump L.even?.113)
     (with-label L.odd?.112 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.115)
     (jump L.__nested.116)
     (with-label L.__nested.115 (halt 0))
     (with-label L.__nested.116 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.even?.113)
     (with-label L.even?.113 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.117)
     (jump L.__nested.118)
     (with-label L.__nested.117 (halt 1))
     (with-label L.__nested.118 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.odd?.112))
   (begin
     (halt 0)
     (with-label L.zero.119 (set! r15 rdi))
     (set! r15 rsi)
     (set! r15 rdx)
     (set! r15 rcx)
     (halt 0))
   (begin
     (set! r15 L.id.121)
     (set! rdi 5)
     (jump r15)
     (with-label L.id.121 (set! r15 rdi))
     (halt r15))
   (begin
     (set! r15 L.id1.123)
     (set! rdi 5)
     (jump r15)
     (with-label L.id1.123 (set! r15 rdi))
     (halt r15)
     (with-label L.id2.124 (set! r15 rdi))
     (halt r15)))
  ((begin
     (set! rdi 5)
     (jump L.id.126)
     (with-label L.id.126 (set! r15 rdi))
     (set! rax r15)
     (jump done))
   (begin
     (set! rdi 5)
     (jump L.even?.129)
     (with-label L.odd?.128 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.131)
     (jump L.__nested.132)
     (with-label L.__nested.131 (set! rax 0))
     (jump done)
     (with-label L.__nested.132 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.even?.129)
     (with-label L.even?.129 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.133)
     (jump L.__nested.134)
     (with-label L.__nested.133 (set! rax 1))
     (jump done)
     (with-label L.__nested.134 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.odd?.128))
   (begin
     (set! rax 0)
     (jump done)
     (with-label L.zero.135 (set! r15 rdi))
     (set! r15 rsi)
     (set! r15 rdx)
     (set! r15 rcx)
     (set! rax 0)
     (jump done))
   (begin
     (set! r15 L.id.137)
     (set! rdi 5)
     (jump r15)
     (with-label L.id.137 (set! r15 rdi))
     (set! rax r15)
     (jump done))
   (begin
     (set! r15 L.id1.139)
     (set! rdi 5)
     (jump r15)
     (with-label L.id1.139 (set! r15 rdi))
     (set! rax r15)
     (jump done)
     (with-label L.id2.140 (set! r15 rdi))
     (set! rax r15)
     (jump done)))
  ((begin
     (set! rdi 5)
     (jump L.id.142)
     (with-label L.id.142 (set! r15 rdi))
     (set! rax r15)
     (jump done))
   (begin
     (set! rdi 5)
     (jump L.even?.145)
     (with-label L.odd?.144 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.147)
     (jump L.__nested.148)
     (with-label L.__nested.147 (set! rax 0))
     (jump done)
     (with-label L.__nested.148 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.even?.145)
     (with-label L.even?.145 (set! r15 rdi))
     (compare r15 0)
     (jump-if = L.__nested.149)
     (jump L.__nested.150)
     (with-label L.__nested.149 (set! rax 1))
     (jump done)
     (with-label L.__nested.150 (set! r15 r15))
     (set! r15 (+ r15 -1))
     (set! r15 r15)
     (set! rdi r15)
     (jump L.odd?.144))
   (begin
     (set! rax 0)
     (jump done)
     (with-label L.zero.151 (set! r15 rdi))
     (set! r15 rsi)
     (set! r15 rdx)
     (set! r15 rcx)
     (set! rax 0)
     (jump done))
   (begin
     (set! r15 L.id.153)
     (set! rdi 5)
     (jump r15)
     (with-label L.id.153 (set! r15 rdi))
     (set! rax r15)
     (jump done))
   (begin
     (set! r15 L.id1.155)
     (set! rdi 5)
     (jump r15)
     (with-label L.id1.155 (set! r15 rdi))
     (set! rax r15)
     (jump done)
     (with-label L.id2.156 (set! r15 rdi))
     (set! rax r15)
     (jump done)))))
