#lang racket
(require racket/trace)

(define (in-range l h res)
  (if (> l h)
      res                                            ; 完成最后一次,则它的return :  result-value
      (in-range (+ 1 l) h (append res (list l)))))   ; 还要继续递归,则它的return : fn(...) 【Thunk】

(define (range low high)
    (in-range low high '()))

(trace in-range)
(trace range)

(in-range 1 10 '())
(range 1 10)

; -----------------------------------------------------
; 【适用场景】
; 1. 我们的语言支持 Iteration （while）
; 2. 我们的的语言缺少对tail call的优化 （仍会增长stack）

; 【目的】
;  ==>  此时，我们写了一个 recursive 的函数
;       我们可以想个办法使其执行时有着 iterative behavior..

; 【核心】
; 假设有个递归的函数fn， 则可知若fn还要继续递归，则它的return ： fn(...)  --- Thunk
;                              若fn完成最后一次，则它的return :  result-value

(define (thk-range l h res)
  (if (> l h)
      res
      (λ () (thk-range (+ 1 l) h (append res (list l))))))     ; ★妈的,不就是通过 创建thunk  代替了 直接调用f(x)会造成增加一个递归栈。

(define (Thunk? r)
  (not (list? r)))

(define (trampoline fn)
  (λ (args)
    (let [(ret (apply fn args))]
      (if (Thunk? ret)
          (trampoline (ret))
          ret))))

; 高阶函数： map add1 '(1 2 3)  不是会导致先求值add1、然后报错吗？？
;            ====> 求值add1并不报错、而是得到一个 Function 值
(define trop-range (trampoline thk-range))
(define result (trop-range '(1 10 ())))




