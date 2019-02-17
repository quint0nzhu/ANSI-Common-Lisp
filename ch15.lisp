;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.15
;;by quinton from ANSI Common Lisp chapter 15

(defun var? (x);;判断x是否为?开头的变量
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds);;在一个列表中寻找一个变量所关联的值
  (let ((b (assoc x binds)))
    (if b
        (or (binding (cdr b) binds)
            (cdr b)))))

(defun match (x y &optional binds);;接受两棵树，如果能匹配则返回一个关联列表和一个布尔值
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t
     (when (and (consp x) (consp y))
       (multiple-value-bind (b2 yes)
           (match (car x) (car y) binds)
         (and yes (match (cdr x) (cdr y) b2)))))))

(defvar *rules* (make-hash-table));;包含规则的哈希表

(defmacro <- (con &optional ant);;定义事实和规则，事实是没有body部分，规则可以通过证明body部分来证明head部分,返回给定判断式下存储的规则数量
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))

(defun vars-in (expr);;返回expr中的带？的变量列表
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
             (vars-in (cdr expr)))))

(defun change-vars (r);;将变量列表中的变量变换成gensym生成的唯一的变量
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun prove-simple (pred args binds);;不包含与、或、非等逻辑运算的简单证明
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes)
                  (match args (car r)
                    binds)
                (when yes
                  (if (cdr r)
                      (prove (cdr r) b2)
                      (list b2)))))
          (mapcar #'change-vars
                  (gethash pred *rules*))))

(defun prove-and (clauses binds);;包含and的证明过程
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (prove (car clauses) b))
              (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds);;包含or的证明过程
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clause binds);;包含not的证明过程
  (unless (prove clause binds)
    (list binds)))

(defun prove (expr &optional binds);;证明过程，分为简单证明过程和包含与、或、非等逻辑运算的复杂证明过程
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t (prove-simple (car expr) (cdr expr) binds))))

(defmacro with-answer (query &body body);;对绑定进行解析，使之变得易读
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query))
         ,@body))))
