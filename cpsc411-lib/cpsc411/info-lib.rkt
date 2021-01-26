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

(require
 racket/contract
 racket/dict
 racket/set)

(define ((dictof/proc kc) d)
  (and (dict? d)
       (for/and ([(k c) (in-dict kc)])
         (and (dict-has-key? d k)
              ((flat-contract-predicate c) (dict-ref d k))))))

(define ((dictof*/proc kc) d)
  (and (dict? d)
       (set=? (dict-keys d) (dict-keys kc))
       (for/and ([(k c) (in-dict kc)])
         ((flat-contract-predicate c) (dict-ref d k)))))

(define-syntax dictof
  (syntax-rules ()
    [(_ (k c) ...)
     (dictof/proc (list (cons k c) ...))]))

(define-syntax dictof*
  (syntax-rules ()
    [(_ (k c) ...)
     (dictof*/proc (list (cons k c) ...))]))

(define-syntax info/c
  (syntax-rules ()
    [(_ (k c) ...)
     (dictof/proc (list (cons 'k (list/c (transform c))) ...))]))

(require
 (for-syntax
  racket/base
  syntax/parse))
(define-syntax (transform stx)
  (syntax-parse stx
    [(_ (c (~literal ...)))
     #'(listof (transform c))]
    [(_ (c ...))
     #'(list/c (transform c) ...)]
    [(_ c)
     #'c]))

(module+ test
  (require
   racket/function)

  (define register? (dynamic-require "compiler-lib.rkt" 'register?))
  (define aloc? (dynamic-require "compiler-lib.rkt" 'aloc?))
  (define fvar? (dynamic-require "compiler-lib.rkt" 'fvar?))

  (check-true
   ((dictof
     ('locals (list/c (listof symbol?)))
     ('assignment (list/c (listof (list/c symbol? register?)))))
    (info-set '((locals (x y)))
              'assignment
              '((x rax) (y rbx)))))

  (check-false
   ((dictof
     ('locals (list/c (listof symbol?)))
     ('assignment (list/c (listof (list/c symbol? register?)))))
    (info-set '()
              'assignment
              '((x rax) (y rbx)))))

  (check-true
   ((dictof
     ('assignment (list/c (listof (list/c symbol? register?)))))
    (info-set '()
              'assignment
              '((x rax) (y rbx)))))

  (check-true
   ((info/c
     (assignment ((symbol? register?) ...)))
    (info-set '()
              'assignment
              '((x rax) (y rbx)))))
  (check-true
   ((info/c
     (locals (symbol? ...))
     (assignment ((symbol? register?) ...)))
    (info-set '((locals (x y z)))
              'assignment
              '((x rax) (y rbx)))))
  (check-false
   ((info/c
     (locals (symbol? ...))
     (assignment ((symbol? register?) ...)))
    (info-set '()
              'assignment
              '((x rax) (y rbx)))))

  (check-true
   ((let ([loc? (or/c register? fvar?)])
      (info/c
       (locals (aloc? ...))
       (assignment ((aloc? loc?) ...))))
    '((locals (x.1)) (assignment ((x.1 rax)))))))
