(module interp (lib "eopl.ss" "eopl")
  (provide (all-defined-out))
  
  (require "0-lang.scm")
  (require "1-data-structures.scm")
  (require "1-store.scm")
  (require "utils.scm")
  ;;============================================================= 
  (define-datatype Answer Answer?
    ($an-answer
     (val ExpVal?)
     (sto Store?)))

  (define (answer->expval ans)
    (cases Answer ans
      ($an-answer (v sto) v)))

  (define (answer->store ans)
    (cases Answer ans
      ($an-answer (v sto) sto)))
  ;````````````````````````````` for foldl
  (define-datatype ComplexAnswer ComplexAnswer?
    ($a-complex-answer
     (v-list (list-of ExpVal?))
     (sto Store?)))

  (define (complex-answer->vals ans)
    (cases ComplexAnswer ans
      ($a-complex-answer (v-list sto) v-list)))
  
  (define (complex-answer->store ans)
    (cases ComplexAnswer ans
      ($a-complex-answer (v-list sto) sto)))

  ;;============================================================= procedure  
  ;; apply-procedure : Proc * [ExpVal] * store -> Answer
  (define (apply-procedure proc argv-s sto)
    (cases Proc proc
      ($procedure (var-s body-exp saved-env)
                  (value-of body-exp (extend-env* var-s argv-s saved-env) sto))))

  ;;============================================================= 
  ;; value-of :: expression x Env x STORE --> Answer {ExpVal;STORE}
  (define (value-of exp ENV STORE)
    (cases expression exp
      ; ----------------------------------------------- 不含需要被计算的expression
      (const-exp (n)
                 ($an-answer ($num-val n) STORE))
      (var-exp (x)
               ($an-answer (apply-env ENV x) STORE))
      (proc-exp (vars body)
                ($an-answer ($proc-val ($procedure vars body ENV)) STORE))
      ; ----------------------------------------------- 
      (letrec-exp (fid-s bvar-s-s proc-body-s letrec-body)
                  (value-of letrec-body ($extend-env-rec* fid-s bvar-s-s proc-body-s ENV) STORE))
      
      (diff-exp (e1 e2)
                (let [(ans1 (value-of e1 ENV STORE))]
                  (cases Answer ans1 ($an-answer (val1 sto1)
                                                 (let [(ans2 (value-of e2 ENV sto1))]
                                                   (cases Answer ans2 ($an-answer (val2 sto2)
                                                                                  (let [(rval ($num-val (- (expval->num val1) (expval->num val2))))]
                                                                                    ($an-answer rval sto2)))))))))

      (add-exp (e1 e2)
               (let [(ans1 (value-of e1 ENV STORE))]
                 (cases Answer ans1 ($an-answer (val1 sto1)
                                                (let [(ans2 (value-of e2 ENV sto1))]
                                                  (cases Answer ans2 ($an-answer (val2 sto2)
                                                                                 (let [(rval ($num-val (+ (expval->num val1) (expval->num val2))))]
                                                                                   ($an-answer rval sto2)))))))))
      (mult-exp (e1 e2)
                (let [(ans1 (value-of e1 ENV STORE))]
                  (cases Answer ans1 ($an-answer (val1 sto1)
                                                 (let [(ans2 (value-of e2 ENV sto1))]
                                                   (cases Answer ans2 ($an-answer (val2 sto2)
                                                                                  (let [(rval ($num-val (* (expval->num val1) (expval->num val2))))]
                                                                                    ($an-answer rval sto2)))))))))
      ;``````````````````
      (zero?-exp (e1)
                 (let [(ans1 (value-of e1 ENV STORE))]
                   (cases Answer ans1 ($an-answer (val1 sto1)
                                                  (let [(r ($bool-val (= 0 (expval->num val1))))]
                                                    ($an-answer r sto1))))))

      (if-exp (e1 e2 e3)
              (let [(ans1 (value-of e1 ENV STORE))]
                (cases Answer ans1 ($an-answer (val1 sto1)
                                               (if (expval->bool val1)
                                                   (value-of e2 ENV sto1)
                                                   (value-of e3 ENV sto1))))))

      
      (let-exp (var e1 body)
               (let [(ans1 (value-of e1 ENV STORE))]
                 (cases Answer ans1 ($an-answer (val1 sto1)
                                                (value-of body ($extend-env var val1 ENV) sto1)))))

      ; ----------------------------------------------- 
      (call-exp (rator-exp rand-exp-s)
                (let [(ans1 (value-of rator-exp ENV STORE))]
                  (cases Answer ans1
                    ($an-answer (f sto1)
                                ; 定义累积函数g
                                (let [(g (lambda (exp com-ans)
                                           (cases ComplexAnswer com-ans
                                             ($a-complex-answer (vals newest-sto)
                                                                (let [(ans (value-of exp ENV newest-sto))]  ; 这个ENV 很重要、否则没法value-of
                                                                  (cases Answer ans ($an-answer (v sto)
                                                                                                ($a-complex-answer (append vals (list v)) sto))))))))]
                                  ; 使用 foldl g initv lst
                                  (let* [(init-com-ans ($a-complex-answer '() sto1))
                                         (com-ans (foldl g init-com-ans rand-exp-s))]
                                    (cases  ComplexAnswer com-ans
                                      ($a-complex-answer (rator-vals final-sto)
                                                         (apply-procedure (expval->proc f) rator-vals final-sto)))))))))

      ;      (begin-exp (exp1 exps)
      ;                 (letrec
      ;                     ((value-of-begins
      ;                       (lambda (e1 es store)
      ;                         (let ((v1 (value-of e1 env store)))
      ;                           (if (null? es)
      ;                               v1
      ;                               (value-of-begins (car es) (cdr es) (answer->store v1)))))))
      ;                   (value-of-begins exp1 exps store)))
      (begin-exp (exps)
                 ; 定义累积函数g
                 (let [(g (lambda (exp com-ans)
                            (cases ComplexAnswer com-ans
                              ($a-complex-answer (vals newest-sto)
                                                 (let [(ans (value-of exp ENV newest-sto))]  ; 这个ENV 很重要、否则没法value-of
                                                   (cases Answer ans ($an-answer (v sto)
                                                                                 ($a-complex-answer (append vals (list v)) sto))))))))]
                   ; 使用 foldl g initv lst
                   (let* [(init-com-ans ($a-complex-answer '() STORE))
                          (com-ans (foldl g init-com-ans exps))]
                     (cases  ComplexAnswer com-ans
                       ($a-complex-answer (vals final-sto)
                                          ($an-answer (list-last vals) final-sto))))))

      ;`````````````````````````````````````````````````
      (newref-exp (exp1)
                  (let* [(ans1 (value-of exp1 ENV STORE))
                         (rv (newref (answer->expval ans1)))]
                    ($an-answer ($ref-val rv) (get-store))))
      
      (deref-exp (refexp1)
                 (let [(ans1 (value-of refexp1 ENV STORE))]
                   (cases Answer ans1 ($an-answer (v1 sto)
                                                  ($an-answer (deref (expval->ref v1)) sto)))))
      
      (setref-exp (refexp exp1)
                  (let [(ans1 (value-of refexp ENV STORE))]
                    (cases Answer ans1 ($an-answer (refval1 sto1)
                                                   (let [(ans2 (value-of exp1 ENV STORE))]
                                                     (cases Answer ans2 ($an-answer (expval1 sto2)
                                                                                    (begin
                                                                                      (setref! (expval->ref refval1) expval1)
                                                                                      ($an-answer ($num-val 666) (get-store))))))))))
          
      ))

  ;; eval-program :: Program -> ExpVal
  (define (eval-program prog)
    (initialize-store!)
    (cases program prog
      (a-program (expr)
                 (value-of expr (init-env) (get-store)))))
  ;;=============================================================  
  ;; interp :: String -> ExpVal
  (define (interp src)
    (eval-program (scan&parse src)))

  (define run interp)

  )
