#lang eopl

(define the-end-cont
  (lambda (val)
    (begin (eopl:printf "End of computation.~%")
           val)))
;=====================================================================================================================
; (remove-first 'a '(b c d e a g a h))
; (remove-first-ds 'a '(b c d e a g a h))
; (remove-first-proc 'a '(b c d e a g a h))
; (remove-first-inlined 'a '(b c d e a g a h))

; origin
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))
; -------------------------------------------------------------------------------  CPS (ds-rep)
(define remove-first-ds
  (let ()
    
    (define-datatype Continuation Continuation?
      ($end-cont)       
      ($rf-cont
       (symbols symbol?)
       (cont Continuation?)
       ))

    (define (apply-cont k VAL)
      (cases Continuation k
        ($end-cont ()
                   (the-end-cont VAL))
        ($rf-cont (h cont)
                  (apply-cont cont (cons h VAL)))
        ))
    ; ```````````````````
    (define (rf/k s los cont)
      (if (null? los)
          (apply-cont cont '())
          (if (eqv? (car los) s)
              (apply-cont cont (cdr los))
              (rf/k s (cdr los) ($rf-cont (car los) cont)))))
    
    (define (remove-first s los)
      (rf/k s los ($end-cont)))
    
    remove-first))
; -------------------------------------------------------------------------------  CPS (proc-rep)
(define remove-first-proc
  (let ()
    
    (define ($end-cont)
      (lambda (VAL)
        (the-end-cont VAL)))
    (define ($rf-cont h cont)
      (lambda (VAL)
        (apply-cont cont (cons h VAL))))
    
    (define (apply-cont cont val)
      (cont val))
    ; ```````````````````
    (define (rf/k s los cont)
      (if (null? los)
          (apply-cont cont '())
          (if (eqv? (car los) s)
              (apply-cont cont (cdr los))
              (rf/k s (cdr los) ($rf-cont (car los) cont)))))
    
    (define (remove-first s los)
      (rf/k s los ($end-cont)))
    
    remove-first))
; -------------------------------------------------------------------------------  inlined
(define remove-first-inlined
  (let ()
    
    (define (rf/k s los cont)
      (if (null? los)
          (cont '())
          (if (eqv? (car los) s)
              (cont (cdr los))
              (rf/k s (cdr los) (lambda (v1)
                                  (cont (cons (car los) v1)))))))
    
    (define (remove-first s los)
      (rf/k s los (lambda (v)
                    (the-end-cont v))))
    
    remove-first))
; -------------------------------------------------------------------------------  registerized
;(define remove-first-regi
;  (let ()
;    
;    (define the-s 'un)
;    (define the-los 'un)
;    (define the-cont 'un)
;    (define the-val 'un)
;
;    (define (remove-first/k)
;      (if (null? the-los)
;          (begin
;            (set! the-val '())
;            (the-cont))
;          (if (eqv? (car the-los) the-s)
;              (begin
;                (set! the-val (cdr the-los))
;                (the-cont))
;              (let ([val1 (car the-los)]
;                    [saved-cont the-cont])
;                (set! the-los (cdr the-los))
;                (set! the-cont (lambda ()
;                                 (set! the-val (cons val1 the-val))
;                                 (saved-cont)))
;                (remove-first/k)))))
;
;    (define remove-first
;      (lambda (s los)
;        (set! the-s s)
;        (set! the-los los)
;        (set! the-cont (lambda ()
;                         (the-end-cont the-val)))
;        (remove-first/k)))
;
;    remove-first))

(define remove-first-regi
  (let ()
    
    (define %s 'un)
    (define %los 'un)
    (define %cont 'un)
    (define %val 'un)

    (define (rf/k)
      (if (null? %los)
          (begin
            (set! %val '())
            (%cont))
          (if (eqv? (car %los) %s)
              (begin
                (set! %val (cdr %los))
                (%cont))
              (let [(v1 (car %los))
                    (saved-cont %cont)]
                (set! %los (cdr %los))
                (set! %cont (lambda ()
                              (set! %val (cons v1 %val))
                              (saved-cont)))
                (rf/k)))))
    1))
;=====================================================================================================================







































