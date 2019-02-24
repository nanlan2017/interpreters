(module testcases (lib "eopl.ss" "eopl")
  (require "0-lang.scm")
  (provide (all-defined-out))
  
  (define this-is-not-src
    "
  module to-int-maker

  interface
  (
   (ints : [...1...])
   => [...2-iface...]
   )

  body
  module-proc (ints : [...1...])
  [...2-imp...]
  ")





  
  
  (define src-0
    "
  module to-int-maker
  interface
  (
   (ints : [opaque t
                   zero : t
                   succ : (t -> t)
                   pred : (t -> t)
                   is-zero : (t -> bool)]
         )
   => [to-int : (from ints take t -> int)])
  body
  module-proc (ints : [opaque t
                              zero : t
                              succ : (t -> t)
                              pred : (t -> t)
                              is-zero : (t -> bool)])
  [to-int
   = let z? = from ints take is-zero
   in let p = from ints take pred
   in letrec int to-int (x : from ints take t)
   = if (z? x)
   then 0
   else -((to-int (p x)), -1)
   in to-int]

   %%%%%%%%%%%%%%%
   1
")
  )