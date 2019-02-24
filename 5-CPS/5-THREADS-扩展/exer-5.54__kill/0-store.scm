(module store (lib "eopl.ss" "eopl")
  
  (provide initialize-store!
           reference?
           
           newref
           deref
           setref!
           
           Option@instrument-newref
           get-store-as-list)
  ; ===========================================================
  (define reference? integer?)
  ; setting option 
  (define Option@instrument-newref (make-parameter #f))
  
  (define %Store 'uninitialized)
  (define (empty-store) '())
  (define (get-store) %Store)
  (define (initialize-store!) (set! %Store (empty-store)))

  ;; '(foo bar baz) = ((0 foo)(1 bar) (2 baz))
  (define (get-store-as-list)
    (letrec
        [(inner-loop (lambda (sto n)
                       (if (null? sto)
                           '()
                           (cons (list n (car sto))
                                 (inner-loop (cdr sto) (+ n 1))))))]
      (inner-loop %Store 0)))
  

  ;; newref : ExpVal -> Ref
  (define newref
    (lambda (val)
      (let ((next-ref (length %Store)))
        (set! %Store (append %Store (list val)))
        (when (Option@instrument-newref)
          (eopl:printf  "newref: allocating location ~s with initial contents ~s~%" next-ref val))                     
        next-ref)))                     

  ;; deref : Ref -> ExpVal
  (define (deref ref)
    (if (not (integer? ref))
        (eopl:error "Not a ref val" ref)
        (begin
          (list-ref %Store ref))))

  ;; setref! : Ref * ExpVal -> ()
  (define setref!                       
    (lambda (ref val)
      (set! %Store
            (letrec [(setref-inner (lambda (store1 ref1)
                                     (cond
                                       ((null? store1) (report-invalid-reference ref %Store))
                                       ((zero? ref1) (cons val (cdr store1)))
                                       (else (cons (car store1)
                                                   (setref-inner (cdr store1) (- ref1 1)))))))]
              (setref-inner %Store ref)))))

  (define (report-invalid-reference ref the-store)
    (eopl:error 'setref "illegal reference ~s in store ~s" ref the-store))

  )