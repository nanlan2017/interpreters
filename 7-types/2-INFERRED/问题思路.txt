#lang racket


; 这就是  “一个算法问题” (unification)， 没啥特别的。

; 这个计算过程就是：
 我们通过check 对每个 bound variable, subexpression 分配一个 type var (tx, t0 ...)
 然后根据语言的type rules , 得到一组类型约束Equations (比如 t0 = int -> t1 ...)
                              —— 在数据结构上，表示为  [(type-var , type-expression)]
                                        其中： type-var :  t0, t1, tx, tf ..
                                               type-expression : int, int->bool ,             // 不含type-var
                                                                 int -> t0, int -> tx ...     //   含type-var

                 ------------------ 得到equations后，就纯粹和解释器无关了。就是个解 type equations 的问题 -------------------

 最终目标是找到一组 Substitutions (typevar -> type-exp)
 使得一个equations 成立  （比如 t0 ===  int -> t1）
 这个就是 unify (1-pair)
 而使得所有的equations成立，就是 unify , 会得到所有的 type-var的值 【当然也就获得代码中所有var/ 所有exp的类型】

;------------------------------------------------- 需要的steps

1. 把1条 substituion (type-var ~ type-exp) 去应用到一个 type-exp上
      apply-one-subst ::  TExp * Subst = TExp
2. 把一组 substitutions 应用到 type-exp 上

3. unify tyexp1 tyexp2 -> substitution
   即计算使得 tyexp1 = tyexp2 成立的 substitution



; --------------- Subst 数据结构        (宏观思考就把问题简化：不要考虑 exp 互相嵌套的问题、多态类型）
constructors :
  empty  :: Subst
  extend :: TVar * Texp * Subst -> Subst

observer:
  apply-subst 

 
