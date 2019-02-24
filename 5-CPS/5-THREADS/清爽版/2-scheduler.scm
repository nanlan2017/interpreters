(module scheduler (lib "eopl.ss" "eopl")

  (require "0-debug.scm")
  (require "1-queues.scm")
  
  (provide initialize-scheduler!
           set-final-answer! 
           place-on-ready-queue!
           time-expired?
           decrement-timer!

           run-next-thread

           ; global data
           %ready-queue
           %final-answer
           %max-time-slice
           %time-remaining
           )
  ; ================================================================ 
  ;  the state ： components of the scheduler state:  
  (define %ready-queue   'uninitialized)           
  
  (define %max-time-slice    'uninitialized)
  (define %time-remaining    'uninitialized)

  (define %final-answer  'uninitialized)

  ; █ 状态接口: %ready-queue
  ; place-on-ready-queue! : Thread -> Unspecified
  (define (place-on-ready-queue! th)
    (set! %ready-queue (enqueue %ready-queue th)))
  
  ; █ 状态接口： %max-time-slice
  ; initialize-scheduler! : Int -> Unspecified
  (define (initialize-scheduler! ticks)
    (set! %ready-queue (empty-queue))
    (set! %final-answer 'uninitialized)
      
    (set! %max-time-slice ticks)
    (set! %time-remaining %max-time-slice))

  ; █ 状态接口: %time-remaining
  ; time-expired? : () -> Bool 
  (define (time-expired?)
    (zero? %time-remaining))

  ; decrement-timer! : () -> Unspecified
  (define (decrement-timer!)
    (when (@debug) (eopl:printf "--------------- %time-remaining : ~s~n" (- %time-remaining 1)))
    (set! %time-remaining (- %time-remaining 1)))

  ; █ 状态接口: %final-answer
  ; set-final-answer! : ExpVal -> Unspecified 
  (define (set-final-answer! val)
    (set! %final-answer val))

  ; ------------------------------------------------------------------
  ; run-next-thread : () -> FinalAnswer  
  (define (run-next-thread)
    (when (@debug) (eopl:printf "      >>>>>>>>>> run-next-thread..~n"))
    (if (empty? %ready-queue)
        %final-answer
        (dequeue %ready-queue (lambda (first-ready-thread other-ready-threads)
                                ; 从pool中移除
                                (set! %ready-queue other-ready-threads)
                                ; 更新时间、运行起来
                                (set! %time-remaining %max-time-slice) 
                                (first-ready-thread)
                                ))))
  )
