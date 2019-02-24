(module tail-form (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang-in-out.scm")
  (require "transform-CPS-IN-to-CPS-OUT.scm")

  ;  (if (zero? x) (f y) (g z))
  ;     is in tail form, as is
  "if zero?(x) then (f y) else (g z)"
  
  ;  (if b
  ;      (if (zero? x) (f y) (g z))
  ;      (h u))
  "if b then if zero?(x) then (f y) else (g z) else (h u)"
  ;     but
  ;  (+
  ;   (if (zero? x) (f y) (g z))
  ;   37)
  ;     is not in tail form, since the if expression, which contains a procedure call, is not in tail position.
  "+(37, if zero?(x) then (f y) else (g z))"

  ; ======================================================================================================
  (define (all pred? lst)
    (if (null? lst)
        #t
        (and (pred? (car lst))
             (all pred? (cdr lst)))))
  
  ; 举例： 一个Non-tailform的 proc-exp
  ;  λ (x) +(x,1)
  ;  λ (x) (f x)

  (define (simple? exp)
    (cases expression exp
      (const-exp (n)
                 #t)
      (var-exp (var)
               #t)
      (diff-exp (e1 e2)
                (and (simple? e1)
                     (simple? e2)))
      (sum-exp (exps)
               (all simple? exps))
      (zero?-exp (e1)
                 (simple? e1))
      (proc-exp (vars body)
                (tail-form? body))
      (else #f)
      ))

  (define (simple-src? src)
    (cases program (scan&parse src)
      (a-program (exp)
                 (simple? exp))))

  (simple-src? "-(1,x)")
  ; -----------------------------------
  (define (tail-form? exp)
    (cases expression exp     
      (if-exp (e1 e2 e3)
              (and (simple? e1)
                   (tail-form? e2)
                   (tail-form? e3)))
      (let-exp (var e1 body)
               (and (simple? e1)
                    (tail-form? body)))
      (call-exp (rator rands)
                (and (simple? rator)
                     (all simple? rands)))
      (letrec-exp (pids bvars-s body-s letrec-body)
                  (and (all tail-form? body-s)
                       (tail-form? letrec-body)))
      (else (simple? exp))
      ))
  
  (define (tail-src? src)
    (cases program (scan&parse src)
      (a-program (exp)
                 (tail-form? exp))))













      
  )
