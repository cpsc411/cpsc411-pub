#lang racket/base

(require racket/function)
(provide (all-defined-out))

(define (max-int word-size) (sub1 (expt 2 (sub1 word-size))))
(define (min-int word-size) (* -1 (expt 2 (sub1 word-size))))

(define (int-size? word-size i)
  (and (number? i)
       (exact? i)
       (<= (min-int word-size) i (max-int word-size))))

(define (int32? i) (int-size? 32 i))

(define (int64? i) (int-size? 64 i))

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

(define (twos-complement-add word-size n1 n2)
  (handle-overflow word-size (+ n1 n2)))

(define (twos-complement-mul word-size n1 n2)
  (handle-overflow word-size (* n1 n2)))

(define (twos-complement-sub word-size n1 n2)
  (handle-overflow word-size (- n1 n2)))

(define x64-add (curry twos-complement-add 64))

(define x64-sub (curry twos-complement-sub 64))

(define x64-mul (curry twos-complement-mul 64))

;; TODO Looks like behaviour of unsafe ops changed and now these crash
#;(module+ test
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
