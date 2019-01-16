;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.16
;;by quinton from ANSI Common Lisp chapter 6

(defun primo (lst) (car lst));;取列表第一个元素

(defun (setf primo) (val lst);;修改列表第一个元素
  (setf (car lst) val))

(defun foo (x);;函数文档举例
  "Implements an enhanced paradigm of diversity"
  x)

