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

(let ((counter 0));;多个闭包共享一个变量
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (+ counter 1))))

(defun our-complement (f);;取反谓词
  #'(lambda (&rest args)
      (not (apply f args))))

(defun compose (&rest fns);;接受一个或多个函数，返回一个依序将其参数应用的新函数
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns);;接受一个或多个函数，当任一函数返回真时，返回真
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns);;接受一个或多个函数，当所有函数返回真时，返回真
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args);;返回一个期望剩余参数的新函数，本函数参数在左，剩余参数在右
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args);;返回一个期望剩余参数的新函数，本函数参数在右，剩余参数在左
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x) #'(lambda (&rest args) x));;接受一个参数并返回原封不动返回此参数的函数

(defun fibr (n);;斐波那契数列递归版本
  (if (<= n 1)
      1
      (+ (fibr (- n 1))
         (fibr (- n 2)))))

(defun fibi (n);;婓波那契数列迭代版本
  (do ((i n (- i 1))
       (f1 1 (+ f1 f2))
       (f2 1 f1))
      ((<= i 1) f1)))
