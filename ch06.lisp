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

(defun our-adjoin (obj lst &rest args);;自定义的adjoin，member的参数与adjoin一样，所以用args把剩余的参数都传进来
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(defun single? (lst);;判断参数是否为一个只有一个元素的列表
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj);;在列表最后追加一个元素
  (append lst (list obj)))

(defun map-int (fn n);;用一个函数作用于0至n-1,将结果形成一个列表并返回
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun filter (fn lst);;用一个函数作用于列表lst，将非nil结果形成一个列表并返回
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun most (fn lst);;根据函数fn作用于lst中的元素后，返回结果为最大的元素和结果
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun combiner (x);;根据参数类型确定调用哪个函数
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args);;接受任何类型参数，并根据参数类型调用相关函数
  (apply (combiner (car args))
         args))

(defun add-to-list (num lst);;对列表中的每个元素加一个数，并返回原列表
  (mapcar #'(lambda (x)
              (+ x num))
          lst))

(defun make-adder (n);;每次调用返回一个闭包，注意不光是函数，还有一个环境，所以是闭包
  #'(lambda (x)
      (+ x n)))
