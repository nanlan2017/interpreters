# 解释器

此仓库里是我之前学习[《Essential of Programming Language》 (编程语言的本质)](https://book.douban.com/subject/3136252/) 一书时（该书目前尚无中文版）
， 使用Scheme语言（Racket）实现的一系列解释器。  
原书每个章节都讲解、实现了一些新语言特性，但都是最基本的实现。我独立完成了本书的大部分的习题，均为在基础实现的基础上对解释器进行扩展。
  
<font color=red> 该项目原来我放在了另一个仓库下: [nanlan2017/Scheme-repo/EOPL](https://github.com/nanlan2017/Scheme-repo/tree/master/EOPL) 
，现在把它拷贝过来稍加整理、独立建一个仓库。 （所以几乎没有commit记录！） 

--- 

这一系列解释器实现的语言特性包括：

1. 基础部分：
    * 使用sllgen作为解析器生成器，把源程序代码文本解析得到抽象语法树AST。
2. 特性实现：
    * 3-functions       
       实现支持 变量定义、单参函数定义（lambda）; 实现支持递归函数定义。  
       完成了表达式求值。  
       深入探索了变量的作用域及绑定，完成了一个内部的lambda的参数变量匿名化的翻译器（使用de Brujin索引）  
       
    * 4-state  
       引入了“程序状态”，使用一个全局唯一的list代表内存。  
       1. Explicit Reference : 显式引用 。 （如同C里的指针一样，指针变量和普通变量是不同的）  
       2. Implicit Reference : 隐式引用 。 （Java中的引用类型即采取这种方案，对象均创建在堆上，变量不过是一个地址引用）  
       3. 实现了一个语言的内置类型： Pair   
       4. 分别实现了4种函数调用的传参策略： 按值调用、按引用调用； 按名调用、按需调用 （后两种用于惰性求值情况下）  
    
    * 5-CPS  
       将解释器内部实现用CPS风格重写，将计算中的上下文暴露出来；   
       探究了尾递归、trampoline等程序设计技巧；  
       基于CPS实现了简单的异常处理支持，可以 throw/catch 异常； 还支持了 resume异常恢复。  
       基于CPS实现了**简单的多线程与锁机制**。 使用内置的抢占式调度器。
       
    * 6-CPS-more  
       进一步探究如何将普通风格的代码转化为CPS风格；并实现了一个翻译器。  
       
    * 7-types 
       为语言引入了类型（包括int,bool,以及复合的函数类型如 int -> bool）  
       Checked :  实现了type checker，可以对源代码进行类型检查      
       Inferred : 使用unification算法实现了类型自动推导，支持多态类型，如 length :: [a] -> int (求某类型的数组的长度，其中a是任意元素类型)  
       
    * 8-Module  
        引入了模块机制（类似OCaml语言中的module）  
        支持普通的module,  module procedure 和对模块引用进行类型检查
        
    * 9-OOP  
        引入了面向对象机制，支持继承和多态  
        随后实现了带类型的面向对象，并实现了相应的类型检查器。       
       



---


> 一张图说明解释器(interpreter)和编译器(compiler)的区别
> 
> ![avatar](http://images.cnblogs.com/cnblogs_com/sword03/%E8%A7%A3%E9%87%8A%E5%99%A8%E4%B8%8E%E7%BC%96%E8%AF%91%E5%99%A8%E7%9A%84%E5%8C%BA%E5%88%AB.jpg)


