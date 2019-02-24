#lang racket

;1. (lambda (x y) (p (+ 8 x) (q y)))
(λ (x y cont)
  (q y (λ (v1)
         (p (+ 8 y) v1 cont))))
;2. (lambda (x y u v) (+ 1 (f (g x y) (+ u v))))
(λ (x y u v)
  (g x y (λ (v1)
           (f v1 (+ u v) (λ (v2)
                           (cont (+ 1 v2)))))))
;3. (+ 1 (f (g x y) (+ u (h v))))
(g x y (λ (v1)
         (h v (λ (v2)
                (f v1 (+ u v2) (λ (v3)
                                 (cont (+ 1 v3))))))))
;4. (zero? (if a (p x) (p y)))

;5. (zero? (if (f a) (p x) (p y)))
;6. (let ((x (let ((y 8)) (p y)))) x)
;7. (let ((x (if a (p x) (p y)))) x)

