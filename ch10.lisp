;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.03
;;by quinton from ANSI Common Lisp chapter 10

(defun our-toplevel ();;自定义的读取-求值-打印循环
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

(defmacro nil! (x);;将x设置为nil
  (list 'setf x nil))

(defmacro nil!-backquote (x);;将x设置为nil，用后引用符法
  `(setf ,x nil))

(defmacro while (test &rest body);;只要test为真，就依次对body中的语句求值
  `(do ()
       ((not ,test))
     ,@body))

(defun quicksort (vec l r);;快速排序，先选主键(pivot)，再将小于主键的元素放到左边，大于的放到右边，如果两边还存在两个或更多元素时，递归应用这个算法到两边
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
           (while (< (svref vec i) p) (incf i))
           (while (> (svref vec j) p) (decf j))
           (when (<= i j);;注意，这不是循环
             (rotatef (svref vec i) (svref vec j))
             (incf i)
             (decf j)))
    (if (>= (- j l) 1) (quicksort vec l j))
    (if (>= (- r i) 1) (quicksort vec i r)))
  vec)

(defmacro ntimes (n &rest body);;接受一个数字n，并对其主体求值n次，这个版本有问题，x可能会引起变量捕获方面的问题，x可能会和环境中的某些同名变量冲突
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

(defmacro ntimes-gensym (n &rest body);;使用gensym的宏，解决了上一版宏的问题，但还有一个非预期的多重求值的问题，还要改进
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
       ,@body)))

(defmacro ntimes-gensym-gensym (n &rest body);;解决了上一版的多重求值的问题
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

(defmacro cah (lst) `(car ,lst));;car的同义词

