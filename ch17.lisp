;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.18
;;by quinton from ANSI Common Lisp chapter 17

(defun rget1 (prop obj);;递归寻找匹配的方法
  (multiple-value-bind (val in) (gethash prop obj)
    (if in
        (values val in)
        (let ((par (gethash :parent obj)))
          (and par (rget1 prop par))))))

(defun tell (obj message &rest args);;调用某个方法
  (apply (rget1 message obj) obj args))

(defun precedence (obj);;找到obj对象父对象列表
  (labels ((traverse (x)
             (cons x
                   (mapcan #'traverse
                           (gethash :parents x)))))
    (delete-duplicates (traverse obj))))

(defun rget2 (prop obj);;从父对象列表中获得一个方法，每次调用都要先生成父对象的优先级列表
  (dolist (c (precedence obj))
    (multiple-value-bind (val in) (gethash prop c)
      (if in (return (values val in))))))

(defvar *objs* nil);;包含所有当前对象的列表

(defun parents (obj) (gethash :parents obj));;取出一个对象的父类

(defun rget (prop obj);;从父对象列表中获得一个方法，父对象列表已经生成好
  (dolist (c (gethash :preclist obj))
    (multiple-value-bind (val in) (gethash prop c)
      (if in (return (values val in))))))

(defun make-precedence (obj);;构造优先级列表
  (setf (gethash :preclist obj) (precedence obj))
  (dolist (x *objs*)
    (if (member obj (gethash :preclist x))
        (setf (gethash :preclist x) (precedence x)))))

(defun (setf parents) (val obj);;配置一个对象的父类，并重新构造任何需要变动的优先级列表
  (prog1 (setf (gethash :parents obj) val)
    (make-precedence obj)))

(defun obj (&rest parents);;用来创建一个对象及定义其父类
  (let ((obj (make-hash-table)))
    (push obj *objs*)
    (setf (parents obj) parents)
    obj))

(defun run-methods (obj name args);;根据对象和方法的名字调用一个方法
  (let ((meth (rget name obj)))
    (if meth
        (apply meth obj args)
        (error "No ~A method for ~A." name obj))))

(defmacro defprop (name &optional meth?);;定义一个对象的属性名，如果为方法名则meth?为真
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
            `(run-methods obj ',name args)
            `(rget ',name obj)))
     (defun (setf ,name) (val obj)
       (setf (gethash ',name obj) val))))

(defun get-next (obj name);;获得父对象列表中的某个方法
  (some #'(lambda (x) (gethash name x))
        (cdr (gethash :preclist obj))))

(defmacro defmeth (name obj parms &rest body);;定义一个方法
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (setf (gethash ',name ,gobj)
             (labels ((next () (get-next ,gobj ',name)))
               #'(lambda ,parms ,@body))))))

(defun inst (parent);;创建对象实例，只有一个父类，不需要维护对象优先级列表
  (let ((obj (make-hash-table)))
    (setf (gethash :parents obj) parent)
    obj))

(defun rget-new (prop obj);;获得一个对象的方法，集成了对象优先级列表法和递归父对象法
  (let ((prec (gethash :preclist obj)))
    (if prec
        (dolist (c prec)
          (multiple-value-bind (val in) (gethash prop c)
            (if in (return (values val in)))))
        (multiple-value-bind (val in) (gethash prop obj)
          (if in
              (values val in)
              (rget-new prop (gethash :parents obj)))))))

(defun get-next-new (obj name);;获得下一个方法，集成了对象优先级列表法和递归父对象法
  (let ((prec (gethash :preclist obj)))
    (if prec
        (some #'(lambda (x) (gethash name x))
              (cdr prec))
        (get-next-new (gethash :parents obj) name))))

(defmacro parents-v (v);;存放父类的列表，在实例中会保存一个单一的父类
  `(svref ,v 0))

(defmacro layout-v (v);;一个包含属性名字的向量，指出类别或实例的从第四个元素开始的设计
  `(the simple-vector (svref ,v 1)))

(defmacro preclist-v (v);;一个类的优先级列表，在实例中这个表为空
  `(svref ,v 2))

(defun inherit-props-v (classes);;汇整所有新对象的父类，汇整成一个列表，创建一个正确长度的向量，并适当地配置前三个字段
  (delete-duplicates
   (mapcan #'(lambda (c)
               (nconc (coerce (layout-v c) 'list)
                      (inherit-props-v (parents-v c))))
           classes)))

(defun precedence-v (obj);;生成优先级列表，只有类才有，实例没有
  (labels ((traverse (x)
             (cons x
                   (mapcan #'traverse (parents-v x)))))
           (delete-duplicates (traverse obj))))

(defun class-fn-v (parents props);;参数为含有其父类的选择性列表，伴随着零个或多个属性名，返回一个代表类的对象，新类会同时有自已本身的属性名，以及从所有父类继承而来的属性
  (let* ((all (union (inherit-props-v parents) props))
         (obj (make-array (+ (length all) 3)
                          :initial-element :nil)))
    (setf (parents-v obj) parents
          (layout-v obj) (coerce all 'simple-vector)
          (preclist-v obj) (precedence-v obj))
    obj))

(defmacro class-v (&optional parents &rest props);;创建类，class-fn-v的介面
  `(class-fn-v (list ,@parents) ',props))

(defun inst-v (parent);;创建一个对象实例，只有一个父类作为参数
  (let ((obj (copy-seq parent)))
    (setf (parents-v obj) parent
          (preclist-v obj) nil)
    (fill obj :nil :start 3)
    obj))

(declaim (inline lookup-v (setf lookup-v)));;声明两个函数为内联函数

(defun lookup-v (prop obj);;使用一个对象的layout，来获取一个给定名称的属性
  (let ((off (position prop (layout-v obj) :test #'eq)))
    (if off (svref obj (+ off 3)) :nil)))

(defun (setf lookup-v) (val prop obj);;使用一个对象的layout，来设置一个给定名称的属性
  (let ((off (position prop (layout-v obj) :test #'eq)))
    (if off
        (setf (svref obj (+ off 3)) val)
        (error "Can't set ~A of ~A." val obj))))

(defun rget-v (prop obj next?);;根据是否优先级列表为空，分别处理类与对象。如果是类，则遍历其优先级列表，直到我们找到一个对象，如果没有找到返回:nil，如果是对象，我们直接查找属性，并在没找到时递归调用
  (let ((prec (preclist-v obj)))
    (if prec
        (dolist (c (if next? (cdr prec) prec) :nil)
          (let ((val (lookup-v prop c)))
            (unless (eq val :nil) (return val))))
        (let ((val (lookup-v prop obj)))
          (if (eq val :nil)
              (rget-v prop (parents-v obj) nil)
              val)))))

(declaim (inline run-methods-v));;声明为内联函数

(defun run-methods-v (obj name args);;调用一个方法
  (let ((meth (rget-v name obj nil)))
    (if (not (eq meth :nil))
        (apply meth obj args)
        (error "No ~A method for ~A." name obj))))

(defmacro defprop-v (name &optional meth?);;定义一个属性，使其更符合函数式语法
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
            `(run-methods-v obj ',name args)
            `(rget-v ',name obj nil)))
     (defun (setf ,name) (val obj)
       (setf (lookup-v ',name obj) val))))

(defmacro defmeth-v (name obj parms &rest body);;定义一个方法，使其更符合函数式语法
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (defprop-v ,name t)
       (setf (lookup-v ',name ,gobj)
             (labels ((next () (rget-v ,gobj ',name t)))
               #'(lambda ,parms ,@body))))))
