#lang eopl
(provide (all-defined-out))

(require "lang.rkt")
(require "data-structures.rkt")
(require "interp.rkt")
;;=========================================================== (init-env) : [x=1, v=5, x=10]
(define src0-0
  "3"
  )
(define src0-1
  "x"
  )
(define src0-2
  "-(x,3)"
  )
(define src0-3
  "zero?(v)"
  )
(define src0-4
  "let y = 5 in -(x,y)"
  )
(define src0-5
  "if zero? (x) then i else -(v,x)"
  )

;```````````````````` minus,+, *, /
(define src-minus-0
  "minus(-(minus(5),9))"
  )
(define src-add-0
  "+ (3,minus(99))"
  )
(define src-mult-0
  "* (3,minus(99))"
  )
(define src-/-0
  "/ (99,11)"
  )
;```````````````````` cons,car,cdr,null?
(define src-empty
  "emptylist"
  )
(define src-cons-0
  "cons(3,emptylist)"
  )
(define src-cons-1             ; (4 (3))
  "let x = 4 in cons(x,
                    cons(cons(-(x,1),
                             emptylist),
                        emptylist))"
  )
(define src-cons-2
  "car (cons(3,cons(4,emptylist)))"               
  )

(define src-cons-3
  "cdr (cons(3,cons(4,emptylist)))"
  )
(define src-cons-4
  "null? (cons(3,cons(4,emptylist)))"
  )
(define src-cons-5
  "null? (emptylist)"
  )


;```````````````````` list
(define src-list-
  "list ()"
  )
(define src-list-0
  "list (3,4,x)"
  )
(define src-list-1
  "car( list (3,4,x) )"
  )
;```````````````````` cond
(define src-cond-0
  "cond (zero?(x) ==> 101) (zero?(v) ==> 102) (zero? (0) ==> 103) end"
  )
;```````````````````` Bool-exp

;```````````````````` Print
(define src-prt-0
  "(print x)"
  )

;;================================================================================================ Error src
(define srcx-0
  "if zero? then x"
  )

;;===========================================================
(define (run-tests)
  (begin 
;    (interp2 src-empty)
;    (interp2 src-cons-0)
;    (interp2 src-cons-1)
;    (interp2 src-cons-2)
;    (interp2 src-cons-3)
;    (interp2 src-cons-4)
;    (interp2 src-cons-5)
;    (interp2 src-list-)
;    (interp2 src-list-0)
;    (interp2 src-list-1)
;    (interp2 src-cond-0)
    (interp src-prt-0)
    ))
(run-tests)

(expval->list (interp "list (3,4,x)"))

(expval->list (interp
               "let x = 4
                in list(x, -(x,1), -(x,3))"))























