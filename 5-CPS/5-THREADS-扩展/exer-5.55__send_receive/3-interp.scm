(module interp (lib "eopl.ss" "eopl")
  
  (require "0-lang.scm")
  (require "0-debug.scm")
  (require "0-store.scm")
  (require "1-queues.scm")
  (require "1-data-structures.scm")
  (require "1-data-structures-continuation.scm")    
  (require "2-scheduler.scm")
  (require "2-semaphores.scm")     
  
  (provide (all-defined-out))

  (define (apply-procedure/k f argv cont th-ctx)
    (cases Proc f
      ($procedure (var body saved-env)
                  (eval/k body ($extend-env var (newref argv) saved-env) cont th-ctx))))

  (define (apply-unop/k unop1 val cont th-ctx)
    (cases unop unop1
      (zero?-unop () (apply-cont cont ($bool-val (zero? (expval->num val))) th-ctx))        
      (car-unop () (apply-cont cont (car (expval->list val)) th-ctx))
      (cdr-unop () (apply-cont cont ($list-val (cdr (expval->list val))) th-ctx))
      (null?-unop () (apply-cont cont ($bool-val (null? (expval->list val))) th-ctx))
      (print-unop () (begin
                       (eopl:printf "~a~%" (expval->num val))
                       (apply-cont cont ($stub 'PRINT_RET) th-ctx)))
      ))

  (define (run ticks src)
    (value-of-program ticks (scan&parse src)))

  ; ==========================================================================================================
  (define fresh-thread-id
    (let [(sn -1)]
      (lambda ()
        (set! sn (+ 1 sn))
        sn)))

  (define (remove-first pred l)
    (let loop ([passedlst '()]
               [lst l])
      (if (null? lst)
          #f
          (let ([head (car lst)]
                [tail (cdr lst)])
            (if (pred head)
                (append (reverse passedlst) tail)
                (loop (cons head passedlst) tail))))))
  
  ; ---------------------------------------------------- ThreadContext 
  (define %list-of-thread-contexts '())

  ; () -> ThreadContext
  (define (new-thread-context)
    (let ([th-ctx ($a-thread-context
                   (fresh-thread-id) (newref (empty-queue)) (new-mutex))])
      (set! %list-of-thread-contexts (cons th-ctx %list-of-thread-contexts))
      th-ctx))

  (define (remove-thread-context th-context)
    (let ([result (remove-first (lambda (ctx) (eq? ctx th-context))
                                %list-of-thread-contexts)])
      (if result
          (set! %list-of-thread-contexts result)
          (eopl:error 'remove-thread-context "failed to remove thread of id ~s" (thread-context->id th-context)))))

  (define (get-thread-context id)
    (let loop ([th-contexts %list-of-thread-contexts])
      (if (null? th-contexts)
          (eopl:error 'get-thread-context "thread not found for id ~s" id)
          (let ([th-context (car th-contexts)])
            (if (= (thread-context->id th-context) id)
                th-context
                (loop (cdr th-contexts)))))))

  ; ---------------------------------------------------- send/receive  ???
  (define (send-message thread-id message th)
    (let ([th-context (get-thread-context thread-id)])
      (cases ThreadContext th-context
        [$a-thread-context (id ref-to-messages message-mutex)
                           (setref! ref-to-messages (enqueue (deref ref-to-messages) message))
                           (signal-to-mutex message-mutex th)])))

  (define (receive-message cont th-context)
    (cases ThreadContext th-context
      [$a-thread-context (id ref-to-messages message-mutex)
                         (letrec ([handle-message (lambda ()
                                                    (let ([messages (deref ref-to-messages)])
                                                      (if (null? messages)
                                                          (wait-for-mutex message-mutex handle-message)
                                                          (dequeue messages
                                                                   (lambda (message rest-messages)
                                                                     (setref! ref-to-messages rest-messages)
                                                                     (apply-cont cont message th-context))))))])
                           (wait-for-mutex message-mutex handle-message))]))
  
  
  ; ==========================================================================================================
  ; value-of-program : Int * Program -> ExpVal     
  (define (value-of-program timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (eval/k exp1 ($empty-env) ($end-cont-main-thread) (new-thread-context)))))
  
  ; value-of/k : Exp * Env * Cont * ThreadContext -> FinalAnswer  
  (define (eval/k exp env cont th-ctx)      
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont ($num-val num) th-ctx))
      (var-exp (var)
               (apply-cont cont (deref (apply-env env var)) th-ctx))
      (proc-exp (var body)
                (apply-cont cont ($proc-val ($procedure var body env)) th-ctx))
      (let-exp (var exp1 body)
               (eval/k (call-exp (proc-exp var body) exp1) env cont th-ctx))
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (eval/k letrec-body ($extend-env-rec* p-names b-vars p-bodies env) cont th-ctx))
      (const-list-exp (nums)
                      (apply-cont cont ($list-val (map $num-val nums)) th-ctx))
      (diff-exp (exp1 exp2)
                (eval/k exp1 env ($diff1-cont exp2 env cont) th-ctx))
      (if-exp (exp1 exp2 exp3)
              (eval/k exp1 env ($if-test-cont exp2 exp3 env cont) th-ctx))
      (call-exp (rator rand)
                (eval/k rator env ($rator-cont rand env cont) th-ctx))
      (begin-exp (exp exps)    
                 (if (null? exps)
                     (eval/k exp env cont th-ctx)
                     (eval/k (call-exp (proc-exp (fresh-identifier 'dummy) (begin-exp (car exps) (cdr exps))) exp) env cont th-ctx)))
      (set-exp (id exp)
               (eval/k exp env ($set-rhs-cont (apply-env env id) cont) th-ctx))
      (unop-exp (unop1 exp)
                (eval/k exp env ($unop-arg-cont unop1 cont) th-ctx))
      ; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      (spawn-exp (exp)
                 (eval/k exp env ($spawn-cont cont) th-ctx))
      (mutex-exp ()
                 (apply-cont cont ($mutex-val (new-mutex)) th-ctx))
      (wait-exp (exp)
                (eval/k exp env ($wait-cont cont) th-ctx))
      (signal-exp (exp)
                  (eval/k exp env ($signal-cont cont) th-ctx))
      ; ------------------------------------
      (yield-exp ()
                 (place-on-ready-queue! (lambda () (apply-cont cont ($stub 'YIELD-RET) th-ctx)))
                 (run-next-thread))

      (send-exp (th-exp message-exp)
                (eval/k th-exp env ($send1-cont message-exp env cont) th-ctx))

      (receive-exp ()
                   (receive-message cont th-ctx))
      ))
  ; apply-cont : Cont * Exp * ThreadContext -> FinalAnswer  
  (define (apply-cont k VAL th-ctx)
    (if (time-expired?)
        (begin
          (place-on-ready-queue! (lambda () (apply-cont k VAL th-ctx)))
          (run-next-thread))  
        (begin
          (decrement-timer!)
          
          (cases Continuation k       
            ($spawn-cont (cont)
                         (let* ([proc1 (expval->proc VAL)]
                                [ctx1 (new-thread-context)]
                                [thread-id-val ($num-val (thread-context->id ctx1))])
                           (place-on-ready-queue! (lambda () (apply-procedure/k proc1 thread-id-val ($end-cont-subthread) ctx1)))
                           (apply-cont cont thread-id-val th-ctx)))

            ($send1-cont (msg-exp env cont)
                         (eval/k msg-exp env ($send2-cont VAL cont) th-ctx))
            ($send2-cont (thread-id-val cont)
                         (send-message (expval->num thread-id-val) VAL (lambda () (apply-cont cont 'SEND-MSG-RET th-ctx))))
            ; ------------------------------ not %ready-queue, but mutex : wait queue
            ($wait-cont (cont)
                        (let [(mut (expval->mutex VAL))]
                          (wait-for-mutex mut (lambda () (apply-cont cont ($stub 'WAIT-RET) th-ctx)))))

            ($signal-cont (cont)
                          (let [(mut (expval->mutex VAL))]
                            (signal-to-mutex mut (lambda () (apply-cont cont ($stub 'SIGNAL-RET) th-ctx)))))
            ; -----------------------------------------------------------------------------  
            ($end-cont-main-thread ()
                                   (set-final-answer! VAL)
                                   (remove-thread-context th-ctx)
                                   (run-next-thread))  
            ($end-cont-subthread ()
                                 (remove-thread-context th-ctx)
                                 (run-next-thread))

            ; ///////////////////////////////////////////////////////////////////////////////////////////////////
            ; unary-op
            ($unop-arg-cont (unop1 cont)
                            (apply-unop/k unop1 VAL cont th-ctx))
            ; diff-exp
            ($diff1-cont (exp2 saved-env saved-cont)
                         (eval/k exp2 saved-env ($diff2-cont VAL saved-cont) th-ctx))
            ($diff2-cont (val1 saved-cont)
                         (let ((n1 (expval->num val1))
                               (n2 (expval->num VAL)))
                           (apply-cont saved-cont ($num-val (- n1 n2)) th-ctx)))
            ; if
            ($if-test-cont (exp2 exp3 env cont)
                           (if (expval->bool VAL)
                               (eval/k exp2 env cont th-ctx)
                               (eval/k exp3 env cont th-ctx)))
            ; call-exp
            ($rator-cont (rand saved-env saved-cont)
                         (eval/k rand saved-env ($rand-cont VAL saved-cont) th-ctx))
            ($rand-cont (val1 saved-cont)
                        (let ((f (expval->proc val1)))
                          (apply-procedure/k f VAL saved-cont th-ctx)))
            ; set
            ($set-rhs-cont (loc cont)
                           (begin
                             (setref! loc VAL)
                             (apply-cont cont ($stub 'SET-RET) th-ctx)))
            ))))

  )

