#lang racket

(require "../../compiler-lib.rkt")
(provide (all-defined-out))

(define (expand-macros e)
  (define (expander env e)
    (match e
      [`#(,es ...)
       (expander env `(vector ,@es))]
      [`(,op ,es ...)
       (cond
         [(dict-ref env op #f)
          =>
          (lambda (f)
            (expander env (f e)))]
         [else
          (map (lambda (e) (expander env e)) e)])]
      [_ e]))

  (expander
   `((and . ,(lambda (x)
               (match x
                 [`(and)
                  '#t]
                 [`(and ,e)
                  e]
                 [`(and ,e ,es ...)
                  `(if ,e (and ,@es) #f)])))
     (or . ,(lambda (x)
              (match x
                [`(or)
                 '#f]
                [`(or ,e)
                 e]
                [`(or ,e ,es ...)
                 (let ([x (fresh)])
                   `(let ([,x ,e]) (if ,x x (or ,@es))))])))
     (let* . ,(lambda (x)
                (match x
                  [`(let* () ,e)
                   e]
                  [`(let* ([,x ,e] ,binds ...) ,tail)
                   `(let ([,x ,e])
                      (let* ,binds ,tail))])))
     (begin . ,(lambda (x)
                 (match x
                   [`(begin)
                    '(void)]
                   [`(begin ,e)
                    e]
                   [`(begin ,e ,es ...)
                    `(let ([_ ,e])
                       (begin ,@es))])))
     (cond . ,(lambda (stx)
                (match stx
                  [`(cond)
                   #f]
                  [`(cond [else ,e1])
                   e1]
                  [`(cond [,pred ,e1] ,clauses ...)
                   `(if ,pred ,e1 (cond ,@clauses))]
                  [`(cond [,pred] ,clauses ...)
                   (let ([t (fresh)])
                     `(let ([,t ,pred])
                        (if ,t ,t (cond ,@clauses))))])))
     (quote . ,(lambda (stx)
                 (match stx
                   [`(,_ (,sexpr ,sexprs ...))
                    `(cons (quote ,sexpr) (quote ,sexprs))]
                   [`(,_ ())
                    'empty]
                   [`(,_ ,v)
                    v])))
     (vector . ,(lambda (stx)
                  (match stx
                    [`(,_ ,es ...)
                     (let ([tmp (fresh)])
                       `(let ([,tmp (make-vector ,(length es))])
                          (begin
                            ,@(for/list ([e es]
                                         [i (in-naturals)])
                                `(vector-set! ,tmp ,i ,e))
                            ,tmp)))])))
     (define* . ,(lambda (stx)
                   (match stx
                     [`(,_ (,name ,args ...) ,e)
                      `(define ,name (lambda ,args ,e))]
                     [`(,_ (,name ,args ...) ,es ...)
                      `(define ,name
                         (lambda ,args
                           (begin ,@es)))]))))
   e))

;;------------------------------------------------------------------------
;; applyify
;;------------------------------------------------------------------------

(define (applyify p)
  (define (applyify-def def)
    (match def
      [`(define ,n (lambda ,xs ,e))
       `(define ,n (lambda ,xs ,(applyify-expr e)))]))

  (define (applyify-expr e)
    (match e
      [`(let ([,xs ,es] ...) ,e)
       `(let ,(for/list ([x xs]
                         [e es])
                `[,x ,(applyify-expr e)])
          ,(applyify-expr e))]
      [`(letrec ([,xs ,es] ...) ,e)
       `(letrec ,(for/list ([x xs]
                            [e es])
                   `[,x ,(applyify-expr e)])
          ,(applyify-expr e))]
      [`(lambda ,xs ,e)
       `(lambda ,xs ,(applyify-expr e))]
      [`(if ,es ...)
       `(if ,@(for/list ([e es])
                (applyify-expr e)))]
      [`(void)
       e]
      [`(error ,int)
       e]
      [`(,op ,es ...)
       `(call ,(applyify-expr op) ,@(for/list ([e es])
                                      (applyify-expr e)))]
      [_ e]))

  (match p
    [`(module ,defs ... ,e)
     `(module ,@(for/list ([def defs])
                  (applyify-def def))
              ,(applyify-expr e))]))
