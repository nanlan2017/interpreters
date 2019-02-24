(module test-lazy (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  (require "lang.scm")
  (require "data-structures.scm")
  (require "utils.scm")
  (require "interp.scm")

  (define src-0
    "letrec* infinite-loop (x) = (infinite-loop -(x,-1))
     in let f = proc (z) 11
        in (f (infinite-loop 0))"
    )

  (define src-1       ; never stop!
    "letrec* infinite-loop (x) = (infinite-loop -(x,-1))
     in let f = proc (z) z
        in (f (infinite-loop 0))"
    )

  )
