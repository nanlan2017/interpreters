(module scheduler (lib "eopl.ss" "eopl")

  (require "0-debug.scm")
  (require "1-queues.scm")
  (require "1-data-structures.scm")
  
  (provide (all-defined-out))

  ; ------------------------------------------------------------------
  ;  注意： 会减少 %ready-queue 中的线程
  ; run-next-thread : () -> FinalAnswer  
  (define (run-next-thread) 
    (if (empty? %ready-queue)
        %FinalAnswer
        (dequeue %ready-queue (lambda (first-ready-thread other-ready-threads)
                                (set! %ready-queue other-ready-threads)
                                ; deal with first thread
                                (set! %time-remaining %MaxTimeSlice)
                                (set! %current-thread-id (thread->id first-ready-thread)) ; ████ 切换线程时更新 %current-thread-id
                                (run-thread first-ready-thread)))))
                                
  ; ================================================================ 
  ;  the state ： components of the scheduler state:  
  (define %ready-queue   'uninitialized)                   ; ████ 应该是Thread<id,proc>              
  
  (define %MaxTimeSlice    'uninitialized)
  (define %time-remaining    'uninitialized)

  (define %FinalAnswer  'uninitialized)

  (define %current-thread-id 'uninitialized)
  (define %list-of-threads 'uninitialized)                 ; ████ 
  ; ---------------------------
  ; initialize-scheduler! : Int -> ()
  (define (initialize-scheduler! ticks th)
    (set! %ready-queue (empty-queue))
    (set! %FinalAnswer 'uninitialized)
      
    (set! %MaxTimeSlice ticks)
    (set! %time-remaining %MaxTimeSlice)

    (set! %current-thread-id (thread->id th))
    (set! %list-of-threads (list th)))
  ; ---------------------------
  ; %ready-queue
  (define (place-on-ready-queue! th)
    (eopl:printf ".......place-on-ready-queue! thread #~s~n" (thread->id th))
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

  ; %list-of-threads
  (define (set-list-of-threads! val)
    (set! %list-of-threads val))

  )
