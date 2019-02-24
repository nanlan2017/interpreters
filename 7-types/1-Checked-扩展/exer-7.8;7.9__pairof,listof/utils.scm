(module utils (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (define @DEBUG (make-parameter #f))
  ; =====================================================================
  ; (concat-stringlist-with '("a" "b" "c") ",")
  (define (concat-stringlist-with slist separator)
    (let loop ([lst (cdr slist)]
               [acc (car slist)])
      (if (null? lst)
          acc
          (loop (cdr lst) (string-append acc separator (car lst))))))

  ; (intersect '(a b c d) '*)
  (define (intersect lst item)
    (if (or (null? lst)
            (equal? '() (cdr lst)))
        lst
        (cons (car lst) (cons item (intersect (cdr lst) item)))))
            
  )
