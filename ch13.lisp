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

(declaim (type (vector fixnum 20) v));;声明一个变量v，为一维向量，长度20，向量元素类型为fixnum

(setf a (make-array '(1000 1000);;定义一个变量a，1000X1000的二维数组，每个元素为single-float类型，每个元素初始值为1.0s0
                    :element-type 'single-float
                    :initial-element 1.0s0))

(defun sum-elts (a);;计算二维数组中所有元素的和
  (declare (type (simple-array single-float (1000 1000))
                 a))
  (let ((sum 0.0s0))
    (declare (type single-float sum))
    (dotimes (r 1000)
      (dotimes (c 1000)
        (incf sum (aref a r c))))
    sum))

(declaim (type (simple-array fixnum (4 4)) ar));;声明一个变量ar，为简单数组，4X4二维，数组元素类型为fixnum

(defconstant dict (make-array 25000 :fill-pointer 0));;定义一个常量数组，初始指针位置为0

(defun read-words (from);;从文件中读入单词到数组中
  (setf (fill-pointer dict) 0)
  (with-open-file (in from :direction :input)
    (do ((w (read-line in nil :eof)
            (read-line in nil :eof)))
        ((eql w :eof))
      (vector-push w dict))))

(defun xform (fn seq) (map-into seq fn seq));;对序列seq中每个元素调用fn，seq序列保存调用结果

(defun write-words (to);;生成同韵字辞典，单词从后往前按字典序排序，先把每个单词反转，再排序，再把每个单词反转回原来的样子
  (with-open-file (out to :direction :output
                          :if-exists :supersede)
    (map nil #'(lambda (x)
                 (fresh-line out)
                 (princ x out))
         (xform #'nreverse
                (sort (xform #'nreverse dict)
                      #'string<)))))

(defun our-reverse (lst);;lst被逆序收集到rev中，函数返回时变量rev不复存在，但变量rev的值还在，被送回调用函数
  (let ((rev nil))
    (dolist (x lst)
      (push x rev))
    rev))

(defun our-adjoin (obj lst &rest args);;args参数用完后，变量和值都不再使用了
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(defun our-adjoin-s (obj lst &rest args);;args参数用完后，变量和值都不再使用了，所以把它声明成栈上的变量，函数返回后，它自动被释放
  (declare (dynamic-extent args))
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

;;模拟港口工作，代码原型，没有优化

(defparameter *harbor1* nil);;定义港口

(defstruct ship1;;定义轮船
  name flag tons)

(defun enter1 (n f d);;进港口操作
  (push (make-ship1 :name n :flag f :tons d)
        *harbor1*))

(defun find-ship1 (n);;找港口内某条轮船
  (find n *harbor1* :key #'ship1-name))

(defun leave1 (n);;某条轮船出港口
  (setf *harbor1*
        (delete (find-ship1 n) *harbor1*)))

;;代码结束

;;以下为模拟港口工作的第二版代码，有优化

(defstruct ship;;定义轮船结构体
  name flag tons)

(defconstant pool (make-array 1000 :fill-pointer t));;定义一个1000大小的内存池，并使指针指向向量的末尾

(dotimes (i 1000);;对内存池进行初始化，提前生成1000个轮船结构
  (setf (aref pool i) (make-ship)))

(defconstant harbor (make-hash-table :size 1100;;定义港口为一个哈希表，提高查找速度
                                     :test #'eq))

(defun enter (n f d);;入港口操作，如果池中有轮船，直接取出，修改相应属性，再插入哈希表
  (let ((s (if (plusp (length pool))
               (vector-pop pool)
               (make-ship))))
    (setf (ship-name s) n
          (ship-flag s) f
          (ship-tons s) d
          (gethash n harbor) s)))

(defun find-ship (n) (gethash n harbor));;从港口（哈希表）中找到某只轮船

(defun leave (n);;离港操作，把轮船从港口（哈希表）移动内存池中，而不是释放该轮船结构
  (let ((s (gethash n harbor)))
    (remhash n harbor)
    (vector-push s pool)))

;;代码结束
