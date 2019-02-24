#lang eopl
(require racket/trace)

(define (fact-v1 n)
  (if (zero? n) 1 (* n (fact-v1 (- n 1)))))
; -------------------------------------------------------------------
(define-datatype Continuation Continuation?
  ($end-cont)
  ($fact-cont
   (n number?)
   (cont Continuation?))
  )

(define (apply-cont CONT VALUE)
  (cases Continuation CONT
    ($end-cont ()
               (eopl:printf "End of Computation.~n")
               VALUE)
    ($fact-cont (n cont)
                (apply-cont cont (* n VALUE)))
    ))

(define (fact/k n cont)
  (if (zero? n)
      (apply-cont cont 1)
      (fact/k (- n 1) ($fact-cont n cont))))

(define (fact-v2 n)
  (fact/k n ($end-cont)))

; ------------------------------------------------------------------- procedural-representation of Continuation
; inline




