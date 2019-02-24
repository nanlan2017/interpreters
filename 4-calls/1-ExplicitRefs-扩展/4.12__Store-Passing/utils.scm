(module utils (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (define identifier? symbol?)

  ; `````````````````````````````````````````````````` List
  (define (drop lst cnt)
    (let [(size (length lst))]
      (cond
        [(> cnt size) (eopl:error "drop : too much!")]
        [(= cnt size) '()]
        [(= 0 cnt) lst]
        [else (drop (cdr lst) (- cnt 1))])))

  (define (take lst cnt)
    (let [(size (length lst))]
      (cond
        [(> cnt size) (eopl:error "take : too much!")]
        [(= cnt size) lst]
        [(= 0 cnt) '()]
        [else (cons (car lst)
                    (take (cdr lst) (- cnt 1)))])))

  (define (list-last lst)
    (cond
      [(null? lst) '()]
      [(eqv? '() (cdr lst)) (car lst)]
      [else (list-last (cdr lst))]))

  ; f :: e * Acc -> Acc
  (define (foldl f acc lst)
    (if (null? lst)
        acc
        (let [(e1 (car lst))]
          (foldl f (f e1 acc) (cdr lst)))))

  ; f :: e * Acc -> Acc
  (define (foldr f acc lst)
    (if (null? lst)
        acc
        (let [(e1 (car lst))]
          (f e1 (foldr f acc (cdr lst))))))

  (foldl string-append "000" '("a" "b" "c"))  ; "cba000"
  (foldr string-append "000" '("a" "b" "c"))  ; "abc000"
  ; `````````````````````````````````````````````````` 
      
  )
