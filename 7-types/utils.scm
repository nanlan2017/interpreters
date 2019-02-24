(module utils (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  ;------------------------------------------------------------------ list utils
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

  ;; list-index
  (define (location sym los)
    (cond
      [(null? los) #f]
      [(eqv? sym (car los)) 0]  ;; ?? 
      [else (+ 1 (location sym (cdr los)))]))
      
  )
