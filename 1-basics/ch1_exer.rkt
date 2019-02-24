#lang racket


;;=========================================================================== Exer_1.36
(define (g p ps)
  (let [(modified-ps (map (λ (pair) (list (+ 1 (car pair)) (cadr pair)))
                          ps))]
    (cons p modified-ps)))


(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(number-elements '(a b c d e))
;;===========================================================================

; zip的实现
(define (zip lst1 lst2)
  (if (null? lst1)
      '()
      (let* [(h1 (car lst1))
             (r1 (map (λ (e2) (cons h1 e2)) lst2))]
        (append r1 (zip (cdr lst1) lst2)))))
    

(zip '(a b c) '(x y z))

