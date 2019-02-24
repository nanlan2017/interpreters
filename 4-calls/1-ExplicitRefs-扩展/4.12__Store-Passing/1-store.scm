(module store (lib "eopl.ss" "eopl")  
  (require "utils.scm")
  (provide (all-defined-out))
  ;  (provide initialize-store!
  ;           Store?
  ;           get-store
  ;           
  ;           newref
  ;           deref
  ;           setref!)
  ;;======================================  Store
  (define STORE 'uninitialized-store)

  (define Store? list?)

  (define (empty-store)
    '())

  (define (get-store)
    STORE)

  (define (initialize-store!)
    (set! STORE (empty-store)))

  (define (newref val)
    (let [(next-ref (length STORE))]
      (set! STORE (append STORE (list val)))
      next-ref))

  (define (deref i)
    (list-ref STORE i))

  ;; 计算出新store,并set!到STORE
  ;; 无返回值
  (define (setref! idx val)
    (letrec [(setref-inner (lambda (sto i)
                             (cond
                               [(null? sto) (eopl:error "Invalid Reference!")]
                               [(zero? i) (cons val (cdr sto))]
                               [else (cons (car sto) (setref-inner (cdr sto) (- i 1)))])))]
      (set! STORE (setref-inner STORE idx))))
  )
