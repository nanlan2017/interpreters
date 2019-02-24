(module scheduler (lib "eopl.ss" "eopl")

  (require "0-debug.scm")
  (require "1-queues.scm")
  (require "1-data-structures.scm")
  
  (provide initialize-scheduler!
           set-final-answer! 
           place-on-ready-queue!
           time-expired?
           decrement-timer!

           run-next-thread ; ★

           ; global data
           %ready-queue
           %FinalAnswer
           %MaxTimeSlice
           %time-remaining
           )
  ; ================================================================ 
  ;  the state ： components of the scheduler state:  
  (define %ready-queue   'uninitialized)                    
  
  (define %MaxTimeSlice    'uninitialized)
  (define %time-remaining    'uninitialized)

  (define %FinalAnswer  'uninitialized)

  ; place-on-ready-queue! : Thread -> Unspecified
  (define (place-on-ready-queue! th)
    (set! %ready-queue (enqueue %ready-queue th)))
  
  ; initialize-scheduler! : Int -> Unspecified
  (define (initialize-scheduler! ticks)
    (set! %ready-queue (empty-queue))
    (set! %FinalAnswer 'uninitialized)
      
    (set! %MaxTimeSlice ticks)
    (set! %time-remaining %MaxTimeSlice))

  ; time-expired? : () -> Bool 
  (define (time-expired?)
    (zero? %time-remaining))

  ; decrement-timer! : () -> Unspecified
  (define (decrement-timer!)
    (when (@debug) (eopl:printf "--------------- %time-remaining : ~s~n" (- %time-remaining 1)))
    (set! %time-remaining (- %time-remaining 1)))

  ; set-final-answer! : ExpVal -> Unspecified 
  (define (set-final-answer! val)
    (set! %FinalAnswer val))

  ; ------------------------------------------------------------------
  ; run-next-thread : () -> FinalAnswer  
  (define (run-next-thread)
    (when (@debug) (eopl:printf "   >>>>>>>>>>>>>> run-next-thread..~n"))
    (if (empty? %ready-queue)
        %FinalAnswer
        (dequeue %ready-queue (lambda (first-ready-thread other-ready-threads)
                                ; 从pool中移除
                                (set! %ready-queue other-ready-threads)
                                ; 运行
                                (set! %time-remaining (get-time-remaining-of-thread first-ready-thread)) 
                                ((get-thunk-of-thread first-ready-thread))
                                ))))
  )
