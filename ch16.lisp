;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;date: 2019.02.17
;;by quinton from ANSI Common Lisp chapter 16

(defmacro as (tag content);;接受一个字符串，打印在两个标签之间
  `(format t "<~(~A~)>~A</~(~A~)>"
           ',tag ,content ',tag))

(defmacro with (tag &rest body);;接受一个代码体，并将它放在两个标签之间
  `(progn
     (format t "~&<~(~A~)>~%" ',tag)
     ,@body
     (format t "~&</~(~A~)>~%" ',tag)))

(defmacro brs (&optional (n 1));;用于创建多个文本行，控制垂直间距
  (fresh-line)
  (dotimes (i n)
    (princ "<br>"))
  (terpri))

(defun html-file (base);;根据给定的符号返回一个文件名
  (format nil "~(~A~).html" base))

(defmacro page (name title &rest body);;生成整个页面，body中的表达式会被求值
  (let ((ti (gensym)))
    `(with-open-file (*standard-output*
                      (html-file ,name)
                      :direction :output
                      :if-exists :supersede)
       (let ((,ti ,title))
         (as title ,ti)
         (with center
               (as h2 (string-upcase ,ti)))
         (brs 3)
         ,@body))))

(defmacro with-link (dest &rest body);;根据给定的地址dest，创建一个指向HTML文件的链接，链接内部的文本，通过求值body参数中的代码段得出
  `(progn
     (format t "<a href=\"~A\">" (html-file ,dest))
     ,@body
     (princ "</a>")))

(defun link-item (dest text);;接受一个字符串，创建一个带链接的列表项
  (princ "<li>")
  (with-link dest
    (princ text)))

(defun button (dest text);;创建一个被方括号包围的链接
  (princ "[ ")
  (with-link dest
    (princ text))
  (format t " ]~%"))

(defun map3 (fn lst);;对传入列表中的每个元素，用三个参数来调用，分别是元素本身，前一个元素，后一个元素，rec是输助递归函数
  (labels ((rec (curr prev next left)
             (funcall fn curr prev next)
             (when left
               (rec (car left)
                    curr
                    (cadr left)
                    (cdr left)))))
    (when lst
      (rec (car lst) nil (cadr lst) (cdr lst)))))

(defparameter *sections* nil);;定义为网站，包括一些节点

(defstruct item;;定义项结构体，由标识符，名称，文本内容组成
  id title text)

(defstruct section;;定义节点结构体，由标识符，名称，一些项组成
  id title items)

(defmacro defitem (id title text);;生成项结构体
  `(setf ,id
         (make-item :id ',id
                    :title ,title
                    :text ,text)))

(defmacro defsection (id title &rest items);;生成节点结构体
  `(setf ,id
         (make-section :id ',id
                       :title ,title
                       :items (list ,@items))))

(defun defsite (&rest sections);;生成一个网站
  (setf *sections* sections))
