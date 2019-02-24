(module exer2_21 (lib "eopl.ss" "eopl")

  (define-datatype environment environment?
    (empty-env)
    
    (extend-env
     (var symbol?)
     (val value?)
     (env environment?)))

  

     
  )
