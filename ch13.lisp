;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.12
;;by quinton from ANSI Common Lisp chapter 13

(defun length/r (lst);;返回列表长度，递归调用版，注意不是尾递归
  (if (null lst)
      0
      (1+ (length/r (cdr lst)))))

(defun length/rt (lst);;返回列表长度，尾递归版，局部函数len是尾递归函数
  (labels ((len (lst acc)
             (if (null lst)
                 acc
                 (len (cdr lst) (1+ acc)))))
    (len lst 0)))

(declaim (inline single?));;声明single?为内联函数

(defun single? (lst);;判断lst是否为只包含一个元素的列表
  (and (consp lst) (null (cdr lst))))

(defun bar (x);;测试函数，只返回参数
  x)

(defun foo (x);;测试函数，以bar函数的返回结果作为参数，调用内联函数single?
  (single? (bar x)))

(declaim (type fixnum *count*));;声明全局变量的类型，type可写可不写

(defun poly (a b x);;声明局部变量的类型和用the为表达式的值声明类型
  (declare (fixnum a b x))
  (the fixnum (+ (the fixnum (* a (the fixnum (expt x 2))))
                 (the fixnum (* b x)))))

