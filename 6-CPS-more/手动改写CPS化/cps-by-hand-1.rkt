#lang racket

(define (pyth x y)
  (sqrt (+ (* x x) (* y y))))
;-----------------------------------------------

; pyth& x y k = x^2 >>=\x2->
;                y^2 >>=\y2->
;                 x2+y2 >>=\s->
;                  sqrt& >>= k

; pyth& x y k = do
;                x2 <- x^2
;                y2 <- y^2
;                s  <- x2+y2
;                r  <- sqrt& s
;                k r
(define (pyth/k x y cont)
  (*/k x x (λ (x2)
             (*/k y y (λ (y2)
                        (+/k x2 y2 (λ (x2py2)
                                     (sqrt/k x2py2 cont))))))))        ; 显然，把evaluation order都暴露出来了
(define (*/k x y cont)
  (cont (* x y)))

(define (+/k x y cont)
  (cont (+ x y)))        ; 显然，并不是说里面各处用到的函数都是CPS风格的，比如这里面使用了 non-cps +

(define (sqrt/k x cont)
  (cont (sqrt x)))

(pyth/k 3 4 println)
;=========================================================================================================================

(define (fact n)
  (if (= n 0)
      1     ; NOT tail-recursive
      (* n (fact (- n 1)))))
;-----------------------------------------------
;                                        █████████ 不再有任何嵌套的函数调用! (f (g x))   ---->  (g/k x f/k)
;                                        █████████  每一个函数调用都被独立成为一个 continuation中的一个“链节点”
;                                                         最后对它的运算结果使用 cont
(define (fact/k n cont)
  (=/k n 0 (λ (b)
             (if b
                 (cont 1)
                 (-/k n 1 (λ (nm1)
                            (fact/k nm1 (λ (r)
                                          (*/k n r cont)))))))))

(define (-/k x y cont)
  (cont (- x y)))

(define (=/k x y cont)
  (if (= x y)
      (cont #t)
      (cont #f)))

(fact/k 6 println)
;=============================================================================
(define (factorial n)
 (f-aux n 1))           ; factorial , f-aux 具有同一cont (tail-call)

(define (f-aux n a)
 (if (= n 0)
     a        ; tail-recursive
     (f-aux (- n 1) (* n a))))
;-----------------------------------------------
(define (factorial/k n cont)
  (f-aux/k n 1 cont))

(define (f-aux/k n a cont)
  (if (= n 0)
      (cont a)
      (f-aux/k (- n 1) (* n a) cont)))

(factorial/k 6 println)

;=============================================================================
(define foo
  (λ args
    args))
(foo 1 2 3)
; ----------------------------------------------
(define (to-cps f)
 (lambda args
  (let [(r (reverse args))]
   ((car r) (apply f (reverse (cdr r)))))))

(define *& (to-cps *))
(define +& (to-cps +))






