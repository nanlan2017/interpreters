#lang racket


(call/cc (lambda (k)
           (* 5 4))) 

(call/cc (lambda (k)
           (* 5 (k 4))))

(+ 2
   (call/cc (lambda (k)
              (* 5 (k 4)))))


"
-(100,[call/cc (lambda (k)
                 -(5,(k 4)))
               ])
"