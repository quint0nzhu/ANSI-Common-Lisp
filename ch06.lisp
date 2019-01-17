;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.01.16
;;by quinton from ANSI Common Lisp chapter 6

(defun primo (lst) (car lst));;取列表第一个元素

(defun (setf primo) (val lst);;修改列表第一个元素
  (setf (car lst) val))

(defun foo (x);;函数文档举例
  "Implements an enhanced paradigm of diversity"
  x)

(defun our-funcall (fn &rest args);;自定义的funcall，args是不定数量的参数
  (apply fn args))

(defun philosoph (thing &optional property);;在&optional后的参数都是选择性参数，缺省为nil
  (list thing 'is property))

(defun philosoph-fun (thing &optional (property 'fun));;也可以设定缺省参数
  (list thing 'is property))

