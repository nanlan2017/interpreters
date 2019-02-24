(module testcases (lib "eopl.ss" "eopl")
  (require "0-lang.scm")  
  (require "1-type-checker.scm")
  (require "2-interp.scm")

  ;====================================================================

  (define src-0
    "
  module m1
    interface
      [
        a : int
        b : int
        c : int
      ]
    body
      [
        a = 33
        x = -(a,1)    % = 32
        b = -(a,x)    % = 1
        c = -(x,b)   % = 31
      ]

  let a = 10
  in -(-(from m1 take a, from m1 take b),a)     %%%%%%% m1::a - m1::b - 10
"
    )
  (define src-2
    "
  module mybool
  interface
  [opaque t
          true : t
          false : t
          and : (t -> (t -> t))
          not : (t -> t)
          to-bool : (t -> bool)]
  body
  [type t = int
        true = 0
        false = 13
        and = proc (x : t)
        proc (y : t)
        if zero?(x) then y else false
        not = proc (x : t)
        if zero?(x) then false else true
        to-bool = proc (x : t) zero?(x)]
  let true = from mybool take true
  in let false = from mybool take false
  in let and = from mybool take and
  in ((and true) false)
")
  )
