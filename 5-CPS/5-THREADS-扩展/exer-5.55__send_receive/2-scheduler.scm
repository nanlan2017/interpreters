(module scheduler (lib "eopl.ss" "eopl")

  (require "0-debug.scm")
  (require "1-queues.scm")
  (require "1-data-structures.scm")
  
  (provide (all-defined-out))

  ; ------------------------------------------------------------------
  ;  注意： 会减少 %ready-queue 中的线程
  ; run-next-thread : () -> FinalAnswer  
  (define (run-next-thread)
    (when (@debug) (eopl:printf "   >>>>>>>>>>>>>> run-next-thread..~n"))
    
    (if (empty? %ready-queue)
        %FinalAnswer
        (dequeue %ready-queue (lambda (first-ready-thread other-ready-threads)
                                (set! %ready-queue other-ready-threads)
                                ; deal with first thread
                                (set! %time-remaining %MaxTimeSlice) 
                                (first-ready-thread)
                                ))))
  ; ================================================================ 
  ;  the state ： components of the scheduler state:  
  (define %ready-queue   'uninitialized)                      
  
  (define %MaxTimeSlice    'uninitialized)
  (define %time-remaining    'uninitialized)

  (define %FinalAnswer  'uninitialized)
  ; ---------------------------
  ; initialize-scheduler! : Int -> ()
  (define (initialize-scheduler! ticks)
    (set! %ready-queue (empty-queue))
    (set! %FinalAnswer 'uninitialized)
      
    (set! %MaxTimeSlice ticks)
    (set! %time-remaining %MaxTimeSlice))
  ; ---------------------------
  ; %ready-queue
  (define (place-on-ready-queue! th)
    (set! %ready-queue (enqueue %ready-queue th)))

  ; %FinalAnswer
  (define (set-final-answer! val)
    (set! %FinalAnswer val))

  ; %time-remaining
  (define (time-expired?)
    (zero? %time-remaining))

  (define (decrement-timer!)
    (when (@debug) (eopl:printf "--------------- %time-remaining : ~s~n" (- %time-remaining 1)))
    (set! %time-remaining (- %time-remaining 1)))

  )
