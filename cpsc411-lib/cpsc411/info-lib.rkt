#lang racket/base

(require racket/dict)
(provide (all-defined-out))

;; info data structure library
;; ------------------------------------------------------------------------

;; An /info/ is like an association list, but each key must be mapped to a proper
;; list.
;; That is:
;;   info: ((key value) ...)
;;   als:  ((key . value) ...)
;;
;; This makes info's print better, particularly when the value is itself a list.
;; It uses a little more memory, though.

(define (info? v)
  (and
   (list? v)
   (andmap (lambda (v) (and (list? v) (= (length v) 2))) v)))

;; info key -> any
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

;; info key value -> info-field
(define (info-set info key value)
  (dict-set info key (list value)))

;; info key -> info-field
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
